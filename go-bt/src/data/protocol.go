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
	check(err)
	length := binary.BigEndian.Uint32(header[:])

	// check if it's a keep-alive message
	if length == 0 {
		log.Printf("found keep-alive")
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
	MsgBitfield      = 5
	MsgPiece         = 6
)

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
		message = ReadResponse(conn)
		if message.IsKeepAlive() {
			log.Printf("keep-alive received\n")
		}
		switch message.MessageId {
		case MsgUnchoke:
			handler.IsChocked = false
		case MsgBitfield:
			handler.UpdatePeerPieces(&message)
		default:
			log.Printf("unknown messageId=%d", message.MessageId)
		}
		// request a block
		req := Request(1, 0)
		conn.Write(req.ToBytes())
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
