package data_test

import (
	"crypto/sha1"
	"encoding/hex"
	"go-bt/data"
	"os"
	"reflect"
	"testing"

	"github.com/marksamman/bencode"
)

func TestGetHandshake(t *testing.T) {
	file, err := os.Open("testdata/ubuntu.torrent")
	defer file.Close()
	if err != nil {
		panic(err)
	}
	dict, err := bencode.Decode(file)
	if err != nil {
		panic(err)
	}
	infoHash := sha1.Sum(bencode.Encode(dict["info"]))
	var peerId [20]byte
	copy(peerId[:], "12345678901234567890")
	handshake := data.GetHanshake(infoHash, peerId)
	expectedHexBytes := "13426974546f7272656e742070726f746f636f6c00000000000000009e638562ab1c1fced9def142864cdd5a7019e1aa3132333435363738393031323334353637383930"
	if hexBytes := hex.EncodeToString(handshake.ToBytes()); hexBytes != expectedHexBytes {
		t.Errorf("%v", hexBytes)
	}
}

func TestGetPiecesFromBitField(t *testing.T) {
	bitfield := []byte{128}
	pieces := data.XXXGetPiecesFromBitField(bitfield)
	expected := map[uint32]bool{0: true}
	if !reflect.DeepEqual(pieces, expected) {
		for key := range pieces {
			t.Errorf("key: %d", key)
		}
		t.Errorf("only expecting the first piece")
	}
}
