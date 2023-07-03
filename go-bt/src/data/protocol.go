package data

import (
	"bytes"
	"encoding/binary"
)

type Handshake struct {
	Pstrlen byte
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
	buffer := new(bytes.Buffer)
	binary.Write(buffer, binary.BigEndian, len(handshake.Pstr))
	handshake.Pstrlen = buffer.Bytes()[0]

	return handshake
}