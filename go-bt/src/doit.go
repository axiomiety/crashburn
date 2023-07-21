package main

/*
https://releases.ubuntu.com/23.04/ubuntu-23.04-live-server-amd64.iso.torrent?_ga=2.8965308.823290078.1689502166-1552750137.1687512839 -o ubuntu.torrent
*/

import (
	"encoding/binary"
	"flag"
	"fmt"
	"go-bt/data"
	"io"
	"log"
	"net"
	"os"
	"time"
)

func check(err error) {
	if err != nil {
		panic(err)
	}
}

func Foo() {
	fmt.Printf("%v\n", data.ParseTorrentFile("ubuntu.torrent"))

	file, _ := os.Open("tracker.response.beencoded")
	defer file.Close()
	bodyBytes, _ := io.ReadAll(file)
	fmt.Printf("%v\n", data.ParseTrackerResponse(bodyBytes))
}

func Main() {
	torrent := data.ParseTorrentFile("ubuntu.torrent")
	trackerResponse := torrent.QueryTracker()

	var peerId [20]byte
	copy(peerId[:], "12345678901234567890")
	log.Printf("piece length (bytes): %d\n", torrent.Info.PieceLength)
	timeout := 5 * time.Second
	for _, peer := range trackerResponse.Peers {
		handshake := data.GetHanshake(torrent.InfoHash, peerId)
		if peer.IP == "" && peer.Port == 0 {
			continue
		}
		ip := net.ParseIP(peer.IP)
		if ip == nil {
			continue
		}
		// need logic for both IPv4 and IPv6
		var conn net.Conn
		var err error
		if ip.To4() != nil {
			connStr := fmt.Sprintf("%s:%d", peer.IP, peer.Port)
			log.Printf("attempting to connect to ipv4 %s", connStr)
			conn, err = net.DialTimeout("tcp", connStr, timeout)

		} else {
			connStr := fmt.Sprintf("[%s]:%d", peer.IP, peer.Port)
			log.Printf("attempting to connect to ipv6 %s", connStr)
			conn, err = net.DialTimeout("tcp6", connStr, timeout)
		}
		check(err)
		defer conn.Close()
		conn.Write(handshake.ToBytes())
		msg := data.Request(0, 0)
		conn.Write(msg.ToBytes())
		log.Println("handshake + request")
		piece := make([]byte, torrent.Info.PieceLength)
		// TODO: extract to a global var
		respBuf := make([]byte, 2^15)
		totBytes := 0
		seen := false
		for {
			numBytesRead, err := conn.Read(respBuf)
			if !seen {
				blockLen := binary.BigEndian.Uint32(respBuf[:4]) - 9
				// big endian
				index := respBuf[4]
				pieceOffset := binary.BigEndian.Uint32(respBuf[5:9])
				log.Printf("blockLen: %d, index:%d, pieceOffset:%d", blockLen, index, pieceOffset)
				seen = true
			}

			totBytes += numBytesRead
			log.Printf("number of bytes read: %d, total: %d\n", numBytesRead, totBytes)
			if err != nil {
				if err != io.EOF {
					fmt.Println("read error:", err)
				}
				break
			}
			piece = append(piece, respBuf...)
			if len(piece) == 2^15 {
				log.Println("full piece!")
			}
		}
		time.Sleep(2 * time.Second)
	}

	time.Sleep(10 * time.Second)

}

func main() {
	registry := map[string]func(){
		"Foo":  Foo,
		"Main": Main,
	}
	var funcFlag = flag.String("n", "Foo", "function to run")
	flag.Parse()
	log.Printf("calling %s", *funcFlag)
	registry[*funcFlag]()
}
