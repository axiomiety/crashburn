package data

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"log"
	"net"
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

func Request(index uint16, offset uint16) Message {
	length := make([]byte, 4)
	binary.BigEndian.PutUint32(length, 13)
	payload := make([]byte, 9)
	binary.BigEndian.PutUint16(payload[0:], index)
	binary.BigEndian.PutUint16(payload[2:], offset)
	// we request a fixed length - doesn't matter
	binary.BigEndian.PutUint32(payload[4:], 2^15)
	return Message{
		Length:    [4]byte(length),
		MessageId: 6,
		Payload:   payload,
	}
}

type KeepAlive struct {
	Message
}

func ReadHandshake(reader io.Reader) Handshake {
	// read 1 byte for the len of Pstr
	// then read 49 + len
	buf := make([]byte, 1)
	_, err := io.ReadFull(reader, buf)
	check(err)
	pstrLength := buf[0]
	buf = make([]byte, 49+pstrLength-1)
	_, err = io.ReadFull(reader, buf)
	check(err)
	return Handshake{
		PstrLen:  pstrLength,
		Pstr:     buf[1:pstrLength],
		Reserved: [8]byte(buf[pstrLength : pstrLength+8]),
		InfoHash: [20]byte(buf[pstrLength+8 : pstrLength+8+20]),
		PeerId:   [20]byte(buf[pstrLength+8+20:]),
	}
}

func (m *Message) IsKeepAlive() bool {
	return binary.BigEndian.Uint32((m.Length[:])) == 0
}

func ReadResponse(reader io.Reader) Message {
	header := make([]byte, 4)
	_, err := io.ReadFull(reader, header)
	if err != io.EOF {
		check(err)
	}
	length := binary.BigEndian.Uint32(header[:])

	// check if it's a keep-alive message
	// those have no id and no payload
	if length == 0 {
		return Message{}
	}

	buffer := make([]byte, length)
	_, err = io.ReadFull(reader, buffer)
	msg := Message{
		Length:    [4]byte(header),
		MessageId: buffer[0],
	}
	if len(buffer) > 1 {
		msg.Payload = buffer[1:]
	}
	return msg
}

type PeerHandler struct {
	PeerId    [20]byte
	IsChocked bool
	// use a bitfield - maybe?
	// we check unint16 is enough when we parse the tracker
	AvailablePieces map[uint16]bool
}

func connectToPeer(peer Peer) (net.Conn, error) {

	var conn net.Conn
	var err error

	ip := net.ParseIP(peer.IP)
	if ip == nil {
		return nil, errors.New(fmt.Sprintf("can't parse IP %s", peer.IP))
	}
	// need logic for both IPv4 and IPv6

	timeout := 5 * time.Second
	if ip.To4() != nil {
		connStr := fmt.Sprintf("%s:%d", peer.IP, peer.Port)
		log.Printf("attempting to connect to ipv4 %s", connStr)
		conn, err = net.DialTimeout("tcp", connStr, timeout)

	} else {
		connStr := fmt.Sprintf("[%s]:%d", peer.IP, peer.Port)
		log.Printf("attempting to connect to ipv6 %s", connStr)
		conn, err = net.DialTimeout("tcp6", connStr, timeout)
	}
	return conn, err
}

const (
	MsgChoke    byte = 0
	MsgUnchoke       = 1
	MsgHave          = 4
	MsgBitfield      = 5
	MsgPiece         = 7
)

func GetPiecesFromBitField(bitfield []byte) map[uint16]bool {
	pieces := make(map[uint16]bool)
	for offset, row := range bitfield {
		for i := 0; i < 8; i++ {
			if row&(1<<i) > 0 {
				pieces[uint16(offset*8+(7-i))] = true
			}
		}
	}
	return pieces
}

func (handler *PeerHandler) UpdatePeerPieces(m *Message) {
}

func (handler *PeerHandler) HandlePeer(peer Peer, handshake Handshake) {
	conn, err := connectToPeer(peer)
	if err != nil {
		log.Printf("can't connect to peer: %s\n", err)
		return
	}
	defer conn.Close()

	// start with a handshake
	conn.Write(handshake.ToBytes())
	// we should receive one!
	respHandshake := ReadHandshake(conn)
	handler.PeerId = respHandshake.PeerId

	var message Message
	for {
		if handler.IsChocked {
			log.Println("peer is chocked, moving on")
			break
		}
		message = ReadResponse(conn)
		if message.IsKeepAlive() {
			log.Printf("keep-alive received\n")
		}
		switch message.MessageId {
		case MsgChoke:
			handler.IsChocked = true
		case MsgUnchoke:
			handler.IsChocked = false
		case MsgHave:
			handler.AvailablePieces = ExtractPiecesFromBitfield(message.Payload)
		case MsgBitfield:
			// we don't need to merge - the bitfield has every piece the peer holds
			handler.AvailablePieces = GetPiecesFromBitField(message.Payload)
		case MsgPiece:
			//<len=0009+X><id=7><index><begin><block>
			pieceIndex := binary.BigEndian.Uint32(message.Payload[:4])
			beginOffset := binary.BigEndian.Uint32(message.Payload[4:8])
			log.Printf("piece: idx=%d, begin=%d\n", pieceIndex, beginOffset)
		default:
			log.Printf("unknown messageId=%d", message.MessageId)
		}

		// request a piece
		for pieceIdx := range handler.AvailablePieces {
			req := Request(pieceIdx, 0)
			conn.Write(req.ToBytes())
			break
		}
	}
}

//func Request(index uint32, offset uint32, pieceLength uint32) Message {
/*

 - find a piece we don't have
 - handshake with a random peer
 - check state of peer (choked/unchocked)
 	- this might be a random delay like 1s?
	- if choked, move to the next peer
 - request piece

 length of last piece = total legnth % piece length?
*/
