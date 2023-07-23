package data

import (
	"bytes"
	"encoding/binary"
	"io"
	"log"
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
	messageLength := buf[0]
	buf = make([]byte, messageLength-1)
	_, err = io.ReadFull(reader, buf)
	check(err)
	pstrOffset := messageLength - 49 - 1
	return Handshake{
		PstrLen:  messageLength,
		Pstr:     buf[1:pstrOffset],
		Reserved: [8]byte(buf[pstrOffset : pstrOffset+8]),
		InfoHash: [20]byte(buf[pstrOffset+8 : pstrOffset+8+20]),
		PeerId:   [20]byte(buf[pstrOffset+8+20:]),
	}
}

func ReadResponse(reader io.Reader) []byte {
	header := make([]byte, 4)
	_, err := io.ReadFull(reader, header)
	check(err)
	length := binary.BigEndian.Uint32(header[:])

	// check if it's a keep-alive message
	if length == 0 {
		log.Printf("found keep-alive")
		return []byte{}
	}

	buffer := make([]byte, length)
	_, err = io.ReadFull(reader, buffer)
	return buffer
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
