package data

import (
	"bytes"
	"encoding/binary"
	"encoding/gob"
)

type Handshake struct {
	PstrLen  [4]byte
	Pstr     []byte
	Reserved [8]byte
	InfoHash [20]byte
	PeerId   [20]byte
}

func GetHanshake(infoHash [20]byte, peerId [20]byte) Handshake {
	b := make([]byte, 4)
	binary.BigEndian.PutUint32(b, uint32(len("BitTorrent Protocol")))
	pstrLen := [4]byte{}
	copy(pstrLen[:], b)
	handshake := Handshake{
		PstrLen:  pstrLen,
		Pstr:     []byte("BitTorrent protocol"),
		InfoHash: infoHash,
		PeerId:   peerId,
	}

	return handshake
}

func (h *Handshake) ToBytes() []byte {
	buffer := new(bytes.Buffer)
	enc := gob.NewEncoder(buffer)
	enc.Encode(h)
	return buffer.Bytes()
}

func (h *Message) ToBytes() []byte {
	buffer := new(bytes.Buffer)
	enc := gob.NewEncoder(buffer)
	enc.Encode(h)
	return buffer.Bytes()
}

type Message struct {
	Length    [4]byte
	MessageId byte
	Payload   []byte
}

func Request(index uint32, offset uint32, length uint32) Message {
	lenBytes := make([]byte, 4)
	binary.BigEndian.PutUint32(lenBytes, 13)
	payload := make([]byte, 4)
	payload = binary.BigEndian.AppendUint32(payload, index)
	payload = binary.BigEndian.AppendUint32(payload, offset)
	payload = binary.BigEndian.AppendUint32(payload, length)
	msg := Message{
		Length:    [4]byte(lenBytes),
		MessageId: 6,
		Payload:   payload,
	}
	return msg
}

/*

 - find a piece we don't have
 - handshake with a random peer
 - check state of peer (choked/unchocked)
 	- this might be a random delay like 1s?
	- if choked, move to the next peer
 - request piece

*/
