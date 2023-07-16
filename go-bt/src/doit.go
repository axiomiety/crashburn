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
	log.Println("here")
	for _, peer := range trackerResponse.Peers {
		handshake := data.GetHanshake(torrent.InfoHash, peerId)
		if peer.IP == "" && peer.Port == 0 {
			continue
		}
		connStr := fmt.Sprintf("[%s]:%d", peer.IP, peer.Port)
		log.Printf("attempting to connect to %s", connStr)
		conn, err := net.Dial("tcp6", connStr)
		check(err)
		defer conn.Close()
		conn.Write(handshake.ToBytes())
		msg := data.Request(0, 0, 128)
		conn.Write(msg.ToBytes())
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
