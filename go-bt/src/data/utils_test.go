package data_test

import (
	"bytes"
	"go-bt/data"
	"testing"
)

func TestBitfield(t *testing.T) {
	//bitfield := []byte{0b11111111, 0b00000001}
	bitfield := []byte{0xff, 0x1}
	pieces := data.ExtractPiecesFromBitfield(bitfield)
	if len(pieces) != 9 {
		t.Errorf("Bitfield %v should convert to 9 keys in map: %v", bitfield, pieces)
	}
	bitfield_ := data.MakeBitfieldFromPieces(16, pieces)
	//t.Errorf("%v %v\n", pieces, bitfield_)
	if !bytes.Equal(bitfield, bitfield_) {
		t.Errorf("Inverse transform from map -> bitfield failed: %v vs %v\n", pieces, bitfield_)
	}
}
