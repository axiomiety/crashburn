package data

import (
	"crypto/sha1"
	"fmt"
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
	PieceLength int64  // in bytes
	Pieces      string // byte string, 20-byte SHA1 for each piece
	Length      int64  // of file, in bytes
}

func check(err error) {
	if err != nil {
		panic(err)
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
				info.PieceLength = val
			}
		case "pieces":
			info.Pieces = "" //value.(string)
		case "length":
			if val, ok := value.(int64); ok {
				info.Length = val
			}
		default:
			log.Printf("ignoring key %s", key)
		}
	}
	return info
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
