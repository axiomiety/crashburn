package data

import (
	"bytes"
	"encoding/binary"
)

type Handshake struct {
	Pstr string
	Reserved [8]byte
	InfoHash [20]byte
	PeerId [20]byte
}

func GetHanshake(infoHash [20]byte, peerId [20]byte) Handshake {
	handshake := Handshake{
		Pstr : "BitTorrent protocol",
		InfoHash : infoHash,
		PeerId : peerId,
	}

	return handshake
}

func (h *Handshake) ToBytes() []byte {
	buffer := new(bytes.Buffer)
	// 4 bytes
	b := make([]byte,4)
	binary.BigEndian.PutUint32(b, uint32(len(h.Pstr)))
	buffer.Write([]byte(h.Pstr))
	buffer.Write(h.Reserved[:])
	buffer.Write(h.InfoHash[:])
	buffer.Write(h.PeerId[:])
	return buffer.Bytes()
}

/*

 - find a piece we don't have
 - handshake with a random peer
 - check state of peer (choked/unchocked)
 	- this might be a random delay like 1s?
	- if choked, move to the next peer
 - request piece

*/