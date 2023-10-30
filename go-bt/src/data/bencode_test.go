package data_test

import (
	"bytes"
	"go-bt/data"
	"testing"
)

func TestBencodeParsing(t *testing.T) {
	r := bytes.NewReader([]byte("i42e"))
	data.ParseBencoded(r)
}
