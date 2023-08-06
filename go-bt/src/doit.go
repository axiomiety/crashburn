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
	"math/rand"
	"os"
	"os/user"
	"sync"
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
	log.Printf("number of pieces: %d\n", torrent.Info.Length/torrent.Info.PieceLength)
	log.Printf("number of peers: %d\n", len(trackerResponse.Peers))

	// see what we have downloaded, if any

	u, _ := user.Current()
	directory := fmt.Sprintf("%s/tmp/blocks", u.HomeDir)
	alreadyDownloaded := data.GetAlreadyDownloadedPieces(directory, torrent.Info.Pieces)
	// shuffle the peers!
	rand.Shuffle(len(trackerResponse.Peers), func(i, j int) {
		trackerResponse.Peers[i], trackerResponse.Peers[j] = trackerResponse.Peers[j], trackerResponse.Peers[i]
	})
	var mu sync.Mutex
	go func() {
		for {
			mu.Lock()
			log.Printf("number of pieces downloaded: %d", len(alreadyDownloaded))
			mu.Unlock()
			time.Sleep(10 * time.Second)
		}
	}()

	var wg sync.WaitGroup
	for _, peer := range trackerResponse.Peers {

		log.Printf("%v\n", peer)
		handshake := data.GetHanshake(torrent.InfoHash, peerId)
		handler := data.PeerHandler{
			AvailablePieces:   make(map[uint32]bool),
			IsChocked:         true,
			IsInterested:      false,
			AlreadyDownloaded: alreadyDownloaded,
			Lock:              &mu,
		}
		wg.Add(1)
		go func() {
			defer wg.Done()
			handler.HandlePeer(peer, handshake, torrent)
		}()

		// start by reading 4 bytes
		time.Sleep(2 * time.Second)
	}
	wg.Wait()

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
