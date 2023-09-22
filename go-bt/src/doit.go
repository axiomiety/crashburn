package main

/*
https://releases.ubuntu.com/23.04/ubuntu-23.04-live-server-amd64.iso.torrent?_ga=2.8965308.823290078.1689502166-1552750137.1687512839 -o ubuntu.torrent
*/

import (
	"encoding/json"
	"flag"
	"fmt"
	"go-bt/data"
	"io"
	"io/ioutil"
	"log"
	"math"
	"math/rand"
	"os"
	"sync"
	"time"
)

func check(err error) {
	if err != nil {
		panic(err)
	}
}

func Foo(_ data.Configuration) {
	fmt.Printf("%v\n", data.ParseTorrentFile("ubuntu.torrent"))

	file, _ := os.Open("tracker.response.beencoded")
	defer file.Close()
	bodyBytes, _ := io.ReadAll(file)
	fmt.Printf("%v\n", data.ParseTrackerResponse(bodyBytes))
}

func Write(conf data.Configuration) {
	torrent := data.ParseTorrentFile(conf.Torrent)
	data.WriteFile(conf.PiecesPath, conf.OutPath, torrent.Info.Length, torrent.GetNumPieces())
}

func Serve(conf data.Configuration) {
	tracker := data.Tracker{}
	tracker.Serve(8088, conf.TorrentsPath)
}

func Create(conf data.Configuration) {
	f, err := os.Open(fmt.Sprintf("%s/%s", conf.Create.Directory, conf.Create.Filename))
	check(err)

	fi, err := f.Stat()
	check(err)

	// that's a multiple of 16kb, but not sure how it's chosen
	pieceLength := 32768
	numberOfPieces := int(math.Ceil(float64(fi.Size()) / float64(pieceLength)))
	fmt.Printf("filesize: %d, numPieces:%d\n", fi.Size(), numberOfPieces)

	// concatenate the 20-bytes hash of each block
}

func Main(conf data.Configuration) {

	torrent := data.ParseTorrentFile(conf.Torrent)
	trackerResponse := torrent.QueryTracker()

	var peerId [20]byte
	copy(peerId[:], []byte(conf.PeerId))
	directory := conf.PiecesPath
	alreadyDownloaded := data.GetAlreadyDownloadedPieces(directory, torrent.Info.Pieces)

	log.Printf("piece length (bytes): %d\n", torrent.Info.PieceLength)
	log.Printf("total number of pieces: %d\n", torrent.Info.Length/torrent.Info.PieceLength)
	log.Printf("number of pieces we have: %d\n", len(alreadyDownloaded))
	log.Printf("number of peers: %d\n", len(trackerResponse.Peers))

	// shuffle the peers!
	rand.Shuffle(len(trackerResponse.Peers), func(i, j int) {
		trackerResponse.Peers[i], trackerResponse.Peers[j] = trackerResponse.Peers[j], trackerResponse.Peers[i]
	})

	// mutex used to update the global state

	var mu sync.Mutex
	var pieceChan = make(chan uint32, 1)

	state := data.State{
		AlreadyDownloaded: alreadyDownloaded,
		PeerHandlers:      make([]data.PeerHandler, conf.MaxPeers),
		Lock:              &mu,
		PieceChan:         pieceChan,
	}

	// status poller
	go state.LogStatus(uint32(torrent.GetNumPieces()))
	// notify all peers once we have a new piece
	go state.PeersNotify()

	var wg sync.WaitGroup
	maxPeers := make(chan uint32, conf.MaxPeers)
	for _, peer := range trackerResponse.Peers {

		log.Printf("%v\n", peer)
		handshake := data.GetHanshake(torrent.InfoHash, peerId)
		handler := data.PeerHandler{
			AvailablePieces: make(map[uint32]bool),
			IsChocked:       true,
			IsInterested:    false,
			State:           &state,
		}
		maxPeers <- 1
		wg.Add(1)
		go func(p data.Peer) {
			defer wg.Done()
			handler.HandlePeer(p, handshake, torrent)
			<-maxPeers
		}(peer)

	}
	wg.Wait()

	time.Sleep(10 * time.Second)

}

func getConf(confPath string) data.Configuration {
	confFile, err := os.Open(confPath)
	if err != nil {
		panic(err)
	}
	defer confFile.Close()
	conf, err := ioutil.ReadAll(confFile)
	if err != nil {
		panic(err)
	}
	myConf := data.Configuration{}
	err = json.Unmarshal(conf, &myConf)
	if err != nil {
		panic(err)
	}
	return myConf
}

func main() {
	registry := map[string]func(data.Configuration){
		"Foo":    Foo,
		"Main":   Main,
		"Write":  Write,
		"Serve":  Serve,
		"Create": Create,
	}
	var funcFlag = flag.String("n", "Foo", "function to run")
	var confFlag = flag.String("c", "", "path to JSON configuration file")
	flag.Parse()
	log.Printf("calling %s", *funcFlag)
	var conf data.Configuration
	if confFlag != nil {
		conf = getConf(*confFlag)
	}
	registry[*funcFlag](conf)
}
