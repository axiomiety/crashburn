package data

import (
	"bytes"
	"encoding/binary"
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
	buffer.Write([]byte{m.MessageId})
	buffer.Write(m.Payload)
	return buffer.Bytes()
}

type Message struct {
	Length    [4]byte
	MessageId byte
	Payload   []byte
}

func Request(index uint32, offset uint32) Message {
	lenBytes := make([]byte, 4)
	binary.BigEndian.PutUint32(lenBytes, 13)
	payload := make([]byte, 4)
	payload = binary.BigEndian.AppendUint32(payload, index)
	payload = binary.BigEndian.AppendUint32(payload, offset)
	// we request a fixed length - doesn't matter
	payload = binary.BigEndian.AppendUint32(payload, 2^15)
	msg := Message{
		Length:    [4]byte(lenBytes),
		MessageId: 6,
		Payload:   payload,
	}
	return msg
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
