package data

import (
	"bytes"
	"encoding/binary"
	"io"
	"log"
	"net"
	"os"
	"time"
)

type Handshake struct {
	PstrLen  byte
	Pstr     []byte
	Reserved [8]byte
	InfoHash [20]byte
	PeerId   [20]byte
}

func GetHanshake(infoHash [20]byte, peerId [20]byte) Handshake {
	pstr := []byte("BitTorrent protocol")
	handshake := Handshake{
		PstrLen:  byte(len(pstr)),
		Pstr:     pstr,
		InfoHash: infoHash,
		PeerId:   peerId,
	}

	return handshake
}

func (h *Handshake) ToBytes() []byte {
	buffer := new(bytes.Buffer)
	buffer.Write([]byte{h.PstrLen})
	buffer.Write(h.Pstr)
	buffer.Write(h.Reserved[:])
	buffer.Write(h.InfoHash[:])
	buffer.Write(h.PeerId[:])
	return buffer.Bytes()
}

func (m *Message) ToBytes() []byte {
	buffer := new(bytes.Buffer)
	buffer.Write(m.Length[:])
	buffer.WriteByte(m.MessageId)
	buffer.Write(m.Payload)
	return buffer.Bytes()
}

type Message struct {
	Length    [4]byte
	MessageId byte
	Payload   []byte
}

func RequestResponse(index uint32, offset uint32, data []byte) Message {
	length := uint32(8 + len(data)/4)
	lenBuf := make([]byte, 4)
	binary.BigEndian.PutUint32(lenBuf, length)
	payload := make([]byte, length)
	binary.BigEndian.PutUint32(payload[0:], index)
	binary.BigEndian.PutUint32(payload[4:], offset)
	copy(payload[8:], data)
	return Message{
		Length:    [4]byte(lenBuf),
		MessageId: MsgPiece,
		Payload:   payload,
	}
}

func Request(index uint32, offset uint32, blockLength uint32) Message {
	log.Printf("requesting %d at offset %d with blockLength %d\n", index, offset, blockLength)
	length := make([]byte, 4)
	binary.BigEndian.PutUint32(length, 13)
	payload := make([]byte, 12)
	binary.BigEndian.PutUint32(payload[0:], index)
	binary.BigEndian.PutUint32(payload[4:], offset)
	binary.BigEndian.PutUint32(payload[8:], blockLength)
	return Message{
		Length:    [4]byte(length),
		MessageId: MsgRequest,
		Payload:   payload,
	}
}

func Bitfield(numberOfPieces uint32, downloadedPieces map[uint32]bool) Message {
	length := make([]byte, 4)
	binary.BigEndian.PutUint32(length, 13)
	return Message{
		Length:    [4]byte(length),
		MessageId: MsgBitfield,
		Payload:   MakeBitfieldFromPieces(numberOfPieces, downloadedPieces),
	}
}

type KeepAlive struct {
	Message
}

func ReadHandshake(reader io.Reader) (Handshake, error) {
	// read 1 byte for the len of Pstr
	// then read 49 + len
	buf := make([]byte, 1)
	_, err := io.ReadFull(reader, buf)
	if err != nil {
		return Handshake{}, err
	}
	pstrLength := buf[0]
	buf = make([]byte, 49+pstrLength-1)
	_, err = io.ReadFull(reader, buf)
	if err != nil {
		return Handshake{}, err
	}
	return Handshake{
		PstrLen:  pstrLength,
		Pstr:     buf[1:pstrLength],
		Reserved: [8]byte(buf[pstrLength : pstrLength+8]),
		InfoHash: [20]byte(buf[pstrLength+8 : pstrLength+8+20]),
		PeerId:   [20]byte(buf[pstrLength+8+20:]),
	}, nil
}

func (m *Message) IsKeepAlive() bool {
	return binary.BigEndian.Uint32((m.Length[:])) == 0 && m.MessageId != MsgTimeout
}

func ReadResponse(conn net.Conn) (Message, error) {
	//log.Println("reading response")
	conn.SetReadDeadline(time.Now().Add(8 * time.Second))
	header := make([]byte, 4)
	_, err := io.ReadFull(conn, header)
	if err != nil && err != io.EOF {
		if os.IsTimeout(err) {
			log.Println("timed out reading length header from client")
			return Message{MessageId: MsgTimeout}, nil
		}
		return Message{}, err
	}
	length := binary.BigEndian.Uint32(header[:])
	//log.Printf("length of message=%d", length)

	// check if it's a keep-alive message
	// those have no id and no payload
	if length == 0 {
		return Message{}, nil
	}

	buffer := make([]byte, length)
	_, err = io.ReadFull(conn, buffer)
	if err != io.EOF {
		if os.IsTimeout(err) {
			log.Println("timed out reading payload from client")
			return Message{MessageId: MsgTimeout}, nil
		}
	}
	msg := Message{
		Length:    [4]byte(header),
		MessageId: buffer[0],
	}
	if len(buffer) > 1 {
		msg.Payload = buffer[1:]
	}
	return msg, nil
}

type PeerHandler struct {
	PeerId          [20]byte
	IsChocked       bool
	IsInterested    bool
	AvailablePieces map[uint32]bool
	TimeoutCount    uint8 // how many times we timed out reading from the peer
	CurrentPiece    uint32
	State           *State
	Conn            net.Conn
	PiecesPath      string
	PiecesHash      map[uint32]string
}

const (
	MsgChoke         byte = 0
	MsgUnchoke            = 1
	MsgInterested         = 2
	MsgNotInterested      = 3
	MsgHave               = 4
	MsgBitfield           = 5
	MsgRequest            = 6
	MsgPiece              = 7
	MsgCancel             = 8
	MsgPort               = 9
	MsgTimeout            = 99 // not part of the spec
)
