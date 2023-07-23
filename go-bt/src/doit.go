package main

/*
https://releases.ubuntu.com/23.04/ubuntu-23.04-live-server-amd64.iso.torrent?_ga=2.8965308.823290078.1689502166-1552750137.1687512839 -o ubuntu.torrent
*/

import (
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
	for idx, peer := range trackerResponse.Peers {
		if idx < 4 {
			continue
		}
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
		respHandshake := ReadHandshake(conn)
		log.Printf("response from peerId %v", respHandshake.PeerId)
		msg := data.Request(1, 0)
		conn.Write(msg.ToBytes())
		log.Println("handshake + request")
		data.ReadResponse(conn)
		// start by reading 4 bytes
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
