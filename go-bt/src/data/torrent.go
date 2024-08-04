package data

import (
	"crypto/sha1"
	"fmt"
	"io"
	"log"
	"os"

	"github.com/marksamman/bencode"
)

type Torrent struct {
	Info         Info
	InfoHash     [20]byte
	Announce     string
	AnnounceList []string
}

type Info struct {
	Name        string
	PieceLength uint64 // bytes per piece
	Pieces      string // byte string, 20-byte SHA1 for each piece
	Length      uint64 // of file, in bytes
}

func check(err error) {
	if err != nil {
		if err != io.EOF {
			panic(err)
		}
	}
}

func parseInfoDict(infoDict map[string]any) Info {
	info := Info{}
	for key, value := range infoDict {
		switch key {
		case "name":
			info.Name = value.(string)
		case "piece length":
			if val, ok := value.(int64); ok {
				info.PieceLength = uint64(val)
			}
		case "pieces":
			info.Pieces = value.(string)
		case "length":
			if val, ok := value.(int64); ok {
				info.Length = uint64(val)
			}
		default:
			log.Printf("ignoring key %s", key)
		}
	}
	log.Printf("length: %d, piece length: %d", info.Length, info.PieceLength)
	// so really, a piece should be around 2^8
	// but just in case...
	if info.Length/info.PieceLength > (1<<32)-1 {
		panic(fmt.Sprintf("number of pieces larger than uint32 (%d) - Peer struct needs to change", info.Length/uint64(info.PieceLength)))
	}
	return info
}

func (t *Torrent) GetNumPieces() uint64 {
	numPieces := t.Info.Length / t.Info.PieceLength
	if t.Info.Length%t.Info.PieceLength > 0 {
		numPieces += 1
	}
	return numPieces
}

func ParseTorrentFile(fname string) Torrent {
	file, err := os.Open(fname)
	check(err)
	dict, err := bencode.Decode(file)
	check(err)
	torrent := Torrent{
		InfoHash: sha1.Sum(bencode.Encode(dict["info"])),
	}

	for key, value := range dict {
		switch key {
		case "announce":
			torrent.Announce = value.(string)
		case "announce-list":
			// this may be an artefact of the bencode library
			for _, val := range value.([]interface{}) {
				torrent.AnnounceList = append(torrent.AnnounceList, fmt.Sprintf("%v", val.([]interface{})[0]))
			}
		case "info":
			torrent.Info = parseInfoDict(value.(map[string]interface{}))
		default:
			log.Printf("ignoring key %s", key)
		}
	}

	return torrent
}

type BETorrent struct {
	InfoHash     [20]byte
	Announce     string   `bencode:"announce"`
	AnnounceList []string `bencode:"annouce-list"`
	Info         BEInfo   `bencode:"info"`
}

type BEInfo struct {
	Name        string `bencode:"name"`
	PieceLength uint64 // bytes per piece
	Pieces      string // byte string, 20-byte SHA1 for each piece
	Length      uint64 `bencode:"length"` // of file(s), in bytes
}
