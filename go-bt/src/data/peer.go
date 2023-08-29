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
	"net"
	"os"
	"sync"
	"time"
)

func connectToPeer(peer Peer) (net.Conn, error) {

	var conn net.Conn
	var err error

	ip := net.ParseIP(peer.IP)
	if ip == nil {
		return nil, errors.New(fmt.Sprintf("can't parse IP %s", peer.IP))
	}

	timeout := 5 * time.Second
	if ip.To4() != nil {
		connStr := fmt.Sprintf("%s:%d", peer.IP, peer.Port)
		log.Printf("attempting to connect to ipv4 %s", connStr)
		conn, err = net.DialTimeout("tcp", connStr, timeout)

	} else {
		connStr := fmt.Sprintf("[%s]:%d", peer.IP, peer.Port)
		log.Printf("attempting to connect to ipv6 %s", connStr)
		conn, err = net.DialTimeout("tcp6", connStr, timeout)
	}
	return conn, err
}

func (handler *PeerHandler) HandlePeer(peer Peer, handshake Handshake, torrent Torrent) {
	peerIdB64 := b64.StdEncoding.EncodeToString([]byte(peer.Id))
	peerLogger := log.New(os.Stdout, fmt.Sprintf("[peer:%s]", peerIdB64), log.Ldate|log.Ltime)
	log.Printf("mapping %s to %s", peer.Id, peerIdB64)
	conn, err := connectToPeer(peer)
	defer conn.Close()
	if err != nil {
		peerLogger.Printf("can't connect to peer: %s\n", err)
		return
	}
	handler.Conn = conn
	peerLogger.Println("connected to peer, sending handshake")
	// start with a handshake
	conn.Write(handshake.ToBytes())
	// we should receive one!
	respHandshake, err := ReadHandshake(conn)
	if err != nil {
		peerLogger.Printf("issue reading handshake from %v, exiting...", peer.Id)
		return
	}
	peerLogger.Println("received handshake")
	// let's send them the pieces we have
	bitfield := Bitfield(handler.State.AlreadyDownloaded)
	conn.Write(bitfield.ToBytes())
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
				offsetChan <- uint32(len(message.Payload) - 8)
				pieceChan <- message.Payload[8:]
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
				peerLogger.Printf("client %v is choked, waiting 5s\n", peer.Id)
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
			}

			idx, err := handler.State.findPieceToRequest(handler.AvailablePieces)
			if err != nil {
				peerLogger.Println("could not find a piece to request!")
				// let's break here and find a peer that has pieces we need
				// our priority is to download pieces right now, not serve them
				// this peer can connect to us again later and hopefully we'll
				// have pieces they're interested in
				cancelFunc()
				break
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
						WritePiece(handler.CurrentPiece, buf)
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
