package data

import (
	"bytes"
	"context"
	"crypto/sha1"
	b64 "encoding/base64"
	"encoding/binary"
	"encoding/hex"
	"errors"
	"fmt"
	"log"
	"math"
	"math/rand"
	"net"
	"os"
	"path/filepath"
	"strconv"
	"sync"
	"time"
)

func (handler *PeerHandler) ListenForPeers(listeningPort int, torrent Torrent, handshake Handshake) {
	addr := GetIPAddr(torrent.Announce, true)
	var listener net.Listener
	var err error
	if ipv4 := addr.(*net.TCPAddr).IP.To4(); ipv4 != nil {
		listener, err = net.Listen("tcp", fmt.Sprintf("%s:%d", addr.(*net.TCPAddr).IP.String(), listeningPort))
		check(err)
	} else {
		listener, err = net.Listen("tcp6", fmt.Sprintf("[%s]:%d", addr.(*net.TCPAddr).IP.String(), listeningPort))
		check(err)

	}
	defer listener.Close()

	for {
		conn, err := listener.Accept()
		if err != nil {
			log.Println("error accepting incoming connections:", err)
			continue
		} else {
			log.Printf("incoming connection! %v\n", conn)
		}
		peerLogger := log.New(os.Stdout, fmt.Sprintf("[peer:%s]", conn.RemoteAddr().String()), log.Ldate|log.Ltime)
		//TODO: need to tell the peer we're not chocked
		go handler.handlePeer(conn, peerLogger, torrent, handshake)
	}
}

func refreshPeers(peerId [20]byte, torrent *Torrent, listeningPort int, peersChan chan []Peer) TrackerResponse {
	log.Println("refreshing tracker")
	trackerResponse := torrent.QueryTracker(peerId, listeningPort)
	log.Printf("found %d peer(s)\n", len(trackerResponse.Peers))
	// shuffle the peers!
	rand.Shuffle(len(trackerResponse.Peers), func(i, j int) {
		trackerResponse.Peers[i], trackerResponse.Peers[j] = trackerResponse.Peers[j], trackerResponse.Peers[i]
	})

	peersChan <- trackerResponse.Peers[:]
	return trackerResponse
}

func RefreshPeers(peerId [20]byte, torrent *Torrent, listeningPort int, peersChan chan []Peer) {

	trackerResponse := refreshPeers(peerId, torrent, listeningPort, peersChan)

	ticker := time.NewTicker(time.Duration(trackerResponse.Interval) / 2 * time.Second)
	for range ticker.C {
		refreshPeers(peerId, torrent, listeningPort, peersChan)
	}
}

func connectToPeer(peer Peer) (net.Conn, error) {

	var conn net.Conn
	var err error

	ip := net.ParseIP("::1") //peer.IP)
	if ip == nil {
		return nil, errors.New(fmt.Sprintf("can't parse IP %v", peer.IP))
	}

	//TODO: check we're not connecting to ourselves!

	timeout := 5 * time.Second
	if ip.To4() != nil {
		connStr := fmt.Sprintf("%s:%d", peer.IP, peer.Port)
		log.Printf("attempting to connect to ipv4 %s", connStr)
		conn, err = net.DialTimeout("tcp", connStr, timeout)

	} else {
		//connStr := fmt.Sprintf("[%s]:%d", peer.IP, peer.Port)
		connStr := fmt.Sprintf("[::1]:%d", peer.Port)
		log.Printf("attempting to connect to ipv6 %s", connStr)
		conn, err = net.DialTimeout("tcp6", connStr, timeout)
	}
	return conn, err
}

func (handler *PeerHandler) getBlockPath(pieceIdx uint32) string {
	baseDir := fmt.Sprintf("%s/%s", handler.PiecesPath, hex.EncodeToString([]byte(handler.PiecesHash[pieceIdx]))[:2])
	return filepath.Join(baseDir, strconv.FormatUint(uint64(pieceIdx), 10))
}

func GetBlock(path string, offset uint32, length uint32) []byte {
	// TODO: check path exists!
	data, err := os.ReadFile(path)
	check(err)
	return data[offset : offset+length]
}

func (handler *PeerHandler) HandlePeer(peer Peer, handshake Handshake, torrent Torrent) {
	var peerId [20]byte
	copy(peerId[:], []byte(peer.Id))
	if bytes.Equal(handler.PeerId[:], peerId[:]) {
		log.Println("that's us! ignoring...")
		return
	}
	log.Printf("%v", peer)
	peerIdB64 := b64.StdEncoding.EncodeToString([]byte(peer.Id))
	peerLogger := log.New(os.Stdout, fmt.Sprintf("[peer:%s]", peerIdB64), log.Ldate|log.Ltime)

	log.Printf("mapping %s to %s", peer.Id, peerIdB64)
	conn, err := connectToPeer(peer)
	if err != nil {
		peerLogger.Printf("can't connect to peer: %s\n", err)
		return
	}
	handler.handlePeer(conn, peerLogger, torrent, handshake)
}

func (handler *PeerHandler) handlePeer(conn net.Conn, peerLogger *log.Logger, torrent Torrent, handshake Handshake) {
	defer conn.Close()
	handler.Conn = conn
	peerLogger.Println("connected to peer, sending handshake")
	// start with a handshake
	conn.Write(handshake.ToBytes())
	// we should receive one!
	respHandshake, err := ReadHandshake(conn)
	if err != nil {
		peerLogger.Println("issue reading handshake, exiting...", err)
		return
	}
	peerLogger.Println("received handshake")
	// let's send them the pieces we have
	bitfield := Bitfield(uint32(torrent.Info.Length/torrent.Info.PieceLength), handler.State.AlreadyDownloaded)
	conn.Write(bitfield.ToBytes())
	peerLogger.Println("sent bitfield")
	handler.PeerId = respHandshake.PeerId

	offsetChan := make(chan uint32)
	pieceChan := make(chan []byte, torrent.Info.PieceLength)
	var wg sync.WaitGroup
	context, cancelFunc := context.WithCancel(context.Background())

	defer func() {
		cancelFunc()
	}()

	wg.Add(1)
	// response reader
	go func() {
		defer wg.Done()
		for {
			select {
			case <-context.Done():
				return
			default:
				// proceed!
			}
			message, err := ReadResponse(conn)
			if err != nil {
				log.Println("issue reading response, exiting")
				break
			}

			if message.IsKeepAlive() {
				log.Printf("keep-alive received\n")
				time.Sleep(5 * time.Second)
				continue
			}
			switch message.MessageId {
			case MsgChoke:
				handler.IsChocked = true
			case MsgUnchoke:
				handler.IsChocked = false
			case MsgInterested:
				handler.IsInterested = true
			case MsgNotInterested:
				peerLogger.Printf("peer is not interested in our pieces")
			case MsgHave:
				handler.AvailablePieces[binary.BigEndian.Uint32(message.Payload)] = true
			case MsgBitfield:
				peerLogger.Printf("updating bitfield\n")
				// we don't need to merge - the bitfield has every piece the peer holds
				handler.AvailablePieces = ExtractPiecesFromBitfield(message.Payload)
			case MsgPiece:
				index := binary.BigEndian.Uint32(message.Payload[:4])
				offset := binary.BigEndian.Uint32(message.Payload[4:8])
				peerLogger.Printf("received piece %d with offset %d\n", index, offset)
				offsetChan <- uint32(len(message.Payload) - 8)
				pieceChan <- message.Payload[8:]
			case MsgRequest:
				index := binary.BigEndian.Uint32(message.Payload[0:])
				offset := binary.BigEndian.Uint32(message.Payload[4:])
				blockLength := binary.BigEndian.Uint32(message.Payload[8:])
				log.Printf("peer requested block %d:%d(%d)\n", index, offset, blockLength)

				data := GetBlock(handler.getBlockPath(index), offset, blockLength)
				resp := RequestResponse(index, offset, data)
				conn.Write(resp.ToBytes())
			case MsgTimeout:
				handler.TimeoutCount += 1
				peerLogger.Printf("timeout count: %d\n", handler.TimeoutCount)
			default:
				peerLogger.Printf("unknown messageId=%d", message.MessageId)
			}
		}
	}()

	wg.Add(1)
	// writer
	go func() {
		defer wg.Done()
		chokeCount := 0
		for {
			if handler.IsChocked {
				peerLogger.Printf("client %v is choked, waiting 5s\n", handler.PeerId)
				time.Sleep(5 * time.Second)
				chokeCount += 1
				if chokeCount > 10 {
					cancelFunc()
				}
				continue
			}

			// does the client have a piece we're interested in?
			if !handler.IsInterested {
				length := make([]byte, 4)
				binary.BigEndian.PutUint32(length, 1)
				msg := Message{
					Length:    [4]byte(length),
					MessageId: MsgInterested,
				}
				conn.Write(msg.ToBytes())
				handler.IsInterested = true
				peerLogger.Println("told peer we're interested!")
				// now we should ideally wait until we receive a bitfield from the client
				continue
			}

			idx, err := handler.State.findPieceToRequest(handler.AvailablePieces)
			if err != nil {
				peerLogger.Println("could not find a piece to request!")
				// let's... wait?
				//TODO: we need to manage peers that haven't given us
				// anything interesting for a while
				time.Sleep(5 * time.Second)
				continue
			}
			handler.CurrentPiece = idx

			offset := uint32(0)
			pieceBuffer := new(bytes.Buffer)
			maxBlockLength := uint32(math.Pow(2, 14) - 1)

			// iterate through a whole piece, block by block
			for {
				//log.Printf("requesting piece=%d at offset=%d", handler.CurrentPiece, offset)
				var remaining uint32
				boundary := torrent.Info.PieceLength
				if handler.CurrentPiece == uint32(torrent.GetNumPieces())-1 {
					peerLogger.Println("requesting last piece!")
					remaining = uint32(torrent.Info.Length%torrent.Info.PieceLength) - offset
					boundary = torrent.Info.Length % torrent.Info.PieceLength
				} else {
					remaining = uint32(torrent.Info.PieceLength) - offset
				}
				var blockLength uint32
				if remaining > maxBlockLength {
					blockLength = maxBlockLength
				} else {
					blockLength = remaining
				}
				req := Request(handler.CurrentPiece, offset, blockLength)
				conn.Write(req.ToBytes())
				// order matters here, otherwise we'll deadlock with the reader!
				offset += <-offsetChan
				pieceBuffer.Write(<-pieceChan)
				// if we're past a piece length, let's hash it
				if offset >= uint32(boundary) {
					buf := pieceBuffer.Bytes()[:boundary]
					pieceHash := sha1.Sum(buf)
					pieceHashStr := hex.EncodeToString(pieceHash[:])
					startIdx := 20 * handler.CurrentPiece
					expectedHashStr := hex.EncodeToString([]byte(torrent.Info.Pieces[startIdx : startIdx+20]))
					peerLogger.Printf("computed: %s", pieceHashStr)
					peerLogger.Printf("versus:   %s", expectedHashStr)
					if pieceHashStr == expectedHashStr {
						WritePiece(handler.PiecesPath, handler.CurrentPiece, buf)
						handler.State.ObtainedPiece(handler.CurrentPiece)
					}
					break
				}
			}

		}
	}()
	wg.Wait()
	log.Println("exiting")
}
