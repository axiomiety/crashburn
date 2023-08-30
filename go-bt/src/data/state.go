package data

import (
	"encoding/binary"
	"errors"
	"log"
	"sync"
	"time"
)

type State struct {
	PeerHandlers      []PeerHandler
	Lock              *sync.Mutex // used to access the state
	AlreadyDownloaded map[uint32]bool
	PieceChan         chan uint32
}

func (state *State) findPieceToRequest(availablePieces map[uint32]bool) (uint32, error) {
	state.Lock.Lock()
	defer state.Lock.Unlock()
	for pieceIdx := range availablePieces {
		_, ok := state.AlreadyDownloaded[pieceIdx]
		if !ok {
			log.Printf("interested in %d\n", pieceIdx)
			return pieceIdx, nil
		}
	}
	return 0, errors.New("no more pieces to request")
}

func (state *State) LogStatus(numPiecesInTorret uint32) {
	for {
		state.Lock.Lock()
		log.Printf("number of pieces downloaded: %d/%d=%.2f%%, ", len(state.AlreadyDownloaded), numPiecesInTorret, float64(100*len(state.AlreadyDownloaded)/int(numPiecesInTorret)))
		state.Lock.Unlock()
		time.Sleep(10 * time.Second)
	}
}

func (state *State) ObtainedPiece(pieceIdx uint32) {
	state.Lock.Lock()
	state.AlreadyDownloaded[pieceIdx] = true
	state.PieceChan <- pieceIdx
	defer state.Lock.Unlock()
}

func (state *State) PeersNotify() {
	for {
		pieceIdx := <-state.PieceChan
		length := make([]byte, 4)
		binary.BigEndian.PutUint32(length, 5)
		pieceIdxBuf := make([]byte, 4)
		binary.BigEndian.PutUint32(pieceIdxBuf, pieceIdx)
		msg := Message{
			Length:    [4]byte(length),
			MessageId: MsgHave,
			Payload:   pieceIdxBuf,
		}
		for _, peer := range state.PeerHandlers {
			go func(peer_ PeerHandler) {
				peer_.Conn.Write(msg.ToBytes())
			}(peer)
		}
	}
}
