package data

import (
	"bytes"
	"crypto/sha1"
	"encoding/hex"
	"fmt"
	"io/fs"
	"log"
	"os"
	"os/user"
	"path/filepath"
	"strconv"
)

func ExtractPiecesFromBitfield(bitfield []byte) map[uint32]bool {
	pieces := make(map[uint32]bool)
	for idx, byte_ := range bitfield {
		for i := 0; i <= 7; i++ {
			if byte_&byte(2^i) > 0 {
				pieces[uint32(idx*8+i)] = true
			}
		}
	}
	return pieces
}

func MakeBitfieldFromPieces(numPieces uint32, pieces map[uint32]bool) []byte {
	var i uint32
	bitfield := make([]byte, numPieces/8)
	for i = 0; i < numPieces; i++ {
		b := bitfield[i/8]
		_, ok := pieces[i]
		if ok {
			b |= byte(2 ^ (i % 8))
		}
	}
	return bitfield
}

func WritePiece(pieceIdx uint32, data []byte) {
	u, _ := user.Current()
	pieceHash := sha1.Sum(data)
	pieceHashStr := hex.EncodeToString(pieceHash[:])
	directory := fmt.Sprintf("%s/tmp/blocks/%s", u.HomeDir, pieceHashStr[:2])
	os.MkdirAll(directory, 0744)
	fname := filepath.Join(directory, strconv.FormatUint(uint64(pieceIdx), 10))
	err := os.WriteFile(fname, data, 0644)
	check(err)
	log.Printf("wrote piece as %s\n", fname)
}

func GetAlreadyDownloadedPieces(path string, piecesHash string) map[uint32]bool {
	m := map[uint32]bool{}
	err := filepath.WalkDir(path, func(path string, entry fs.DirEntry, err error) error {
		if !entry.IsDir() {
			pieceIdx_, err := strconv.ParseUint(entry.Name(), 10, 32)
			pieceIdx := uint32(pieceIdx_)
			check(err)
			data, err := os.ReadFile(path)
			check(err)
			startIdx := 20 * pieceIdx
			pieceHash := sha1.Sum(data)
			if bytes.Equal([]byte(pieceHash[:]), []byte(piecesHash[startIdx:startIdx+20])) {
				log.Printf("piece %d checks out\n", pieceIdx)
				m[pieceIdx] = true
			}
		}
		return nil
	})
	if err != nil {
		log.Fatalf("impossible to walk directories: %s", err)
	}
	return m
}

func piecesToPath(path string) map[uint32]string {
	// piece to path
	m := map[uint32]string{}
	err := filepath.WalkDir(path, func(path string, entry fs.DirEntry, err error) error {
		if !entry.IsDir() {
			pieceIdx_, err := strconv.ParseUint(entry.Name(), 10, 32)
			check(err)
			pieceIdx := uint32(pieceIdx_)
			m[pieceIdx] = path
		}
		return nil
	})
	if err != nil {
		log.Fatalf("impossible to walk directories: %s", err)
	}
	return m
}

func WriteFile(blobsPath string, outPath string, fileSize uint64, numPieces uint64) {
	m := piecesToPath(blobsPath)
	f, err := os.Create(outPath)
	defer f.Close()

	check(err)
	var i uint64
	bytesRemaining := int(fileSize)
	for i = 0; i < numPieces; i++ {
		data, err := os.ReadFile(m[uint32(i)])
		check(err)
		if bytesRemaining >= len(data) {
			f.Write(data)
			bytesRemaining -= len(data)
			log.Printf("bytes remaining: %d - read %s\n", bytesRemaining, m[uint32(i)])
		} else {
			left := len(data) - bytesRemaining
			f.Write(data[:left])
			log.Println("Finished writing")
			break
		}
	}
	f.Sync()
	log.Println("all done")
}
