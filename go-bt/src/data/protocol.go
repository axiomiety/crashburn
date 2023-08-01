package data

import (
	"bytes"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"log"
	"math"
	"net"
	"os"
	"sync"
	"time"
)

type Handshake struct {
	PstrLen  byte
	Pstr     []byte
	Reserved [8]byte
	InfoHash [20]byte
	PeerId   [20]byte
}

func GetHanshake(infoHash [20]byte, peerId [20]byte) Handshake {
	pstr := []byte("BitTorrent protocol")
	handshake := Handshake{
		PstrLen:  byte(len(pstr)),
		Pstr:     pstr,
		InfoHash: infoHash,
		PeerId:   peerId,
	}

	return handshake
}

func (h *Handshake) ToBytes() []byte {
	buffer := new(bytes.Buffer)
	buffer.Write([]byte{h.PstrLen})
	buffer.Write(h.Pstr)
	buffer.Write(h.Reserved[:])
	buffer.Write(h.InfoHash[:])
	buffer.Write(h.PeerId[:])
	return buffer.Bytes()
}

func (m *Message) ToBytes() []byte {
	buffer := new(bytes.Buffer)
	buffer.Write(m.Length[:])
	buffer.WriteByte(m.MessageId)
	buffer.Write(m.Payload)
	return buffer.Bytes()
}

type Message struct {
	Length    [4]byte
	MessageId byte
	Payload   []byte
}

func Request(index uint32, offset uint32) Message {
	length := make([]byte, 4)
	binary.BigEndian.PutUint32(length, 13)
	payload := make([]byte, 12)
	binary.BigEndian.PutUint32(payload[0:], index)
	binary.BigEndian.PutUint32(payload[4:], offset)
	// we request a fixed length - doesn't matter
	binary.BigEndian.PutUint32(payload[8:], uint32(math.Pow(2, 14)-1))
	return Message{
		Length:    [4]byte(length),
		MessageId: 6,
		Payload:   payload,
	}
}

type KeepAlive struct {
	Message
}

func ReadHandshake(reader io.Reader) Handshake {
	// read 1 byte for the len of Pstr
	// then read 49 + len
	buf := make([]byte, 1)
	_, err := io.ReadFull(reader, buf)
	check(err)
	pstrLength := buf[0]
	buf = make([]byte, 49+pstrLength-1)
	_, err = io.ReadFull(reader, buf)
	check(err)
	return Handshake{
		PstrLen:  pstrLength,
		Pstr:     buf[1:pstrLength],
		Reserved: [8]byte(buf[pstrLength : pstrLength+8]),
		InfoHash: [20]byte(buf[pstrLength+8 : pstrLength+8+20]),
		PeerId:   [20]byte(buf[pstrLength+8+20:]),
	}
}

func (m *Message) IsKeepAlive() bool {
	return binary.BigEndian.Uint32((m.Length[:])) == 0 && m.MessageId != MsgTimeout
}

func ReadResponse(conn net.Conn) Message {
	conn.SetReadDeadline(time.Now().Add(8 * time.Second))
	header := make([]byte, 4)
	_, err := io.ReadFull(conn, header)
	if err != io.EOF {
		if os.IsTimeout(err) {
			log.Println("timed out reading length header from client")
			return Message{MessageId: MsgTimeout}
		}
		check(err)
	}
	length := binary.BigEndian.Uint32(header[:])
	log.Printf("length of message=%d", length)

	// check if it's a keep-alive message
	// those have no id and no payload
	if length == 0 {
		return Message{}
	}

	buffer := make([]byte, length)
	_, err = io.ReadFull(conn, buffer)
	if err != io.EOF {
		if os.IsTimeout(err) {
			log.Println("timed out reading payload from client")
			return Message{MessageId: MsgTimeout}
		}
	}
	msg := Message{
		Length:    [4]byte(header),
		MessageId: buffer[0],
	}
	if len(buffer) > 1 {
		msg.Payload = buffer[1:]
	}
	return msg
}

type PeerHandler struct {
	PeerId       [20]byte
	IsChocked    bool
	IsInterested bool
	// use a bitfield - maybe?
	// we check unint16 is enough when we parse the tracker
	AvailablePieces map[uint32]bool
	TimeoutCount    uint8 // how many times we timed out reading from the peer
	CurrentPiece    uint32
	PieceRequested  bool
}

func (handler *PeerHandler) findPieceToRequest() {
	if !handler.PieceRequested {
		for pieceIdx := range handler.AvailablePieces {
			handler.CurrentPiece = pieceIdx
			break
		}
	}
}

func connectToPeer(peer Peer) (net.Conn, error) {

	var conn net.Conn
	var err error

	ip := net.ParseIP(peer.IP)
	if ip == nil {
		return nil, errors.New(fmt.Sprintf("can't parse IP %s", peer.IP))
	}
	// need logic for both IPv4 and IPv6

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

const (
	MsgChoke         byte = 0
	MsgUnchoke            = 1
	MsgInterested         = 2
	MsgNotInterested      = 3
	MsgHave               = 4
	MsgBitfield           = 5
	MsgPiece              = 7
	MsgTimeout            = 99 // not part of the spec
)

func GetPiecesFromBitField(bitfield []byte) map[uint32]bool {
	pieces := make(map[uint32]bool)
	for offset, row := range bitfield {
		for i := 0; i < 8; i++ {
			if row&(1<<i) > 0 {
				pieces[uint32(offset*8+(7-i))] = true
			}
		}
	}
	return pieces
}

func (handler *PeerHandler) HandlePeer(peer Peer, handshake Handshake, torrent Torrent) {
	conn, err := connectToPeer(peer)
	defer conn.Close()
	if err != nil {
		log.Printf("can't connect to peer: %s\n", err)
		return
	}
	log.Println("connected to peer, sending handshake")
	// start with a handshake
	conn.Write(handshake.ToBytes())
	// we should receive one!
	respHandshake := ReadHandshake(conn)
	log.Println("received handshake")
	handler.PeerId = respHandshake.PeerId

	offsetChan := make(chan uint32)
	pieceChan := make(chan []byte, torrent.Info.PieceLength)
	var wg sync.WaitGroup

	wg.Add(1)
	go func() {
		defer wg.Done()
		var message Message
		for {

			message = ReadResponse(conn)

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
				handler.IsInterested = false
			case MsgHave:
				handler.AvailablePieces[uint32(binary.BigEndian.Uint32(message.Payload))] = true
			case MsgBitfield:
				log.Printf("updating bitfield\n")
				// we don't need to merge - the bitfield has every piece the peer holds
				handler.AvailablePieces = GetPiecesFromBitField(message.Payload)
			case MsgPiece:
				//<len=0009+X><id=7><index><begin><block>
				pieceIndex := binary.BigEndian.Uint32(message.Payload[:4])
				beginOffset := binary.BigEndian.Uint32(message.Payload[4:8])
				log.Printf("piece: idx=%d, begin=%d, len=%d\n", pieceIndex, beginOffset, len(message.Payload)-8)
				offsetChan <- uint32(len(message.Payload) - 8)
				pieceChan <- message.Payload[8:]
			case MsgTimeout:
				handler.TimeoutCount += 1
				log.Printf("timeout count: %d\n", handler.TimeoutCount)
			default:
				log.Printf("unknown messageId=%d", message.MessageId)
			}
		}
	}()

	wg.Add(1)
	go func() {
		defer wg.Done()

		for {
			if handler.IsChocked {
				log.Println("client is choked, waiting 5s")
				time.Sleep(5 * time.Second)
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
				log.Println("told peer we're interested!")
			}

			handler.findPieceToRequest()
			offset := uint32(0)
			pieceBuffer := new(bytes.Buffer)
			for {
				log.Printf("requesting piece=%d at offset=%d", handler.CurrentPiece, offset)
				req := Request(handler.CurrentPiece, offset)
				conn.Write(req.ToBytes())
				pieceBuffer.Write(<-pieceChan)
				offset += <-offsetChan
				// if we're past a piece length, let's hash it
				if offset >= uint32(torrent.Info.PieceLength) {

				}
			}

		}
	}()
	// // request a piece
	// if !handler.PieceRequeusted {

	// }
	// for pieceIdx := range handler.AvailablePieces {
	// 	req := Request(pieceIdx, 0)
	// 	log.Printf("requesting piece %d\n", pieceIdx)
	// 	conn.Write(req.ToBytes())
	// 	break
	// }
	wg.Wait()
	log.Println("exiting")
}

//func Request(index uint32, offset uint32, pieceLength uint32) Message {
/*

 - find a piece we don't have
 - handshake with a random peer
 - check state of peer (choked/unchocked)
 	- this might be a random delay like 1s?
	- if choked, move to the next peer
 - request piece

 length of last piece = total legnth % piece length?
*/
