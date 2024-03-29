package main

/*
https://releases.ubuntu.com/23.04/ubuntu-23.04-live-server-amd64.iso.torrent?_ga=2.8965308.823290078.1689502166-1552750137.1687512839 -o ubuntu.torrent
*/

import (
	"bufio"
	"bytes"
	"crypto/sha1"
	"encoding/json"
	"flag"
	"fmt"
	"go-bt/data"
	"io"
	"log"
	"math"
	"os"
	"sync"
	"time"

	"github.com/marksamman/bencode"
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
	var mu sync.Mutex

	tracker := data.Tracker{
		PeerLatestHeartBeat: map[string]int64{},
		Lock:                &mu,
	}

	ticker := time.NewTicker(30 * time.Second)
	go func() {
		for range ticker.C {
			tracker.EjectExpiredPeers(time.Now())
		}
	}()

	tracker.Serve(8088, conf.TorrentsPath)
}

func Create(conf data.Configuration) {
	fname := fmt.Sprintf("%s/%s", conf.Create.Directory, conf.Create.Filename)
	log.Printf("fname: %s\n", fname)
	f, err := os.Open(fname)
	check(err)

	fi, err := f.Stat()
	check(err)

	// that's a multiple of 16kb, but not sure how it's chosen
	pieceLength := 32768
	numberOfPieces := int(math.Ceil(float64(fi.Size()) / float64(pieceLength)))
	fmt.Printf("filesize: %d, numPieces:%d\n", fi.Size(), numberOfPieces)

	// concatenate the 20-bytes hash of each block
	// and create blocks for serving later
	// TODO: this is doubling the size of the file - is there a bette
	os.Mkdir(conf.PiecesPath, 0744)
	reader := bufio.NewReader(f)
	buf := make([]byte, pieceLength)

	hashes := &bytes.Buffer{}

	pieceIdx := 0
	for {
		_, err := reader.Read(buf)
		if err != nil {
			if err != io.EOF {
				log.Fatal(err)
			}
			break
		}
		pieceHash := sha1.Sum(buf)
		hashes.Write(pieceHash[:])
		data.WritePiece(conf.PiecesPath, uint32(pieceIdx), buf)
		pieceIdx += 1
	}
	allHashes := hashes.Bytes()

	info := data.Info{
		Name:        conf.Create.Filename,
		PieceLength: uint64(pieceLength),
		Pieces:      string(allHashes),
		Length:      uint64(fi.Size()),
	}

	torrent := data.Torrent{
		Info:         info,
		InfoHash:     sha1.Sum(bencode.Encode(info)),
		Announce:     "http://localhost:8088",
		AnnounceList: []string{},
	}
	tfile := fmt.Sprintf("%s/file.torrent", conf.Create.Directory)
	torrentFile, err := os.Create(tfile)
	check(err)
	infoDict := map[string]any{
		"name":         "gt-bt 101",
		"piece length": info.PieceLength,
		"pieces":       info.Pieces,
		"length":       info.Length,
	}
	dict := map[string]any{
		"info":     infoDict,
		"announce": torrent.Announce,
	}
	torrentFile.Write(bencode.Encode(dict))
	log.Printf("torrent file written to %s\n", tfile)

}

func Main(conf data.Configuration) {

	torrent := data.ParseTorrentFile(conf.Torrent)

	var peerId [20]byte
	copy(peerId[:], []byte(conf.PeerId))
	directory := conf.PiecesPath
	alreadyDownloaded := data.GetAlreadyDownloadedPieces(directory, torrent.Info.Pieces)

	log.Printf("piece length (bytes): %d\n", torrent.Info.PieceLength)
	log.Printf("total number of pieces: %d\n", torrent.Info.Length/torrent.Info.PieceLength)
	log.Printf("number of pieces we have: %d\n", len(alreadyDownloaded))

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

	// this channel contains slices of peers
	peersChan := make(chan []data.Peer, conf.MaxPeers)
	// we need to listen to incoming connections
	handler := data.PeerHandler{
		AvailablePieces: make(map[uint32]bool),
		IsChocked:       false,
		IsInterested:    false,
		State:           &state,
		PeerId:          peerId,
		PiecesPath:      conf.PiecesPath,
		PiecesHash:      data.HashByPiece(torrent.Info.Pieces),
	}
	handshake := data.GetHanshake(torrent.InfoHash, peerId)
	// TODO: can we listen to the same port for different torrents?
	go handler.ListenForPeers(conf.ListeningPort, torrent, handshake)
	// this is also used for heart-beating the tracker
	go data.RefreshPeers(peerId, &torrent, conf.ListeningPort, peersChan)
	for {
		select {
		case peers := <-peersChan:
			for _, peer := range peers {
				log.Printf("%v\n", peer)

				maxPeers <- 1
				wg.Add(1)
				go func(p data.Peer) {
					defer wg.Done()
					handler.HandlePeer(p, handshake, torrent)
					<-maxPeers
				}(peer)
			}
		}

	}
	// we should ideally have a signal handler that helps break out
	// of the above
	// wg.Wait()
}

func getConf(confPath string) data.Configuration {
	confFile, err := os.Open(confPath)
	if err != nil {
		panic(err)
	}
	defer confFile.Close()
	conf, err := io.ReadAll(confFile)
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
