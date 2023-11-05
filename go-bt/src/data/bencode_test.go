package data_test

import (
	"bytes"
	"go-bt/data"
	"testing"
)

func TestBencodeParsing(t *testing.T) {

	// single integer
	r := bytes.NewReader([]byte("i42e"))
	ret := data.ParseBencoded(r)
	if ret != 42 {
		t.Errorf("expected 42, got %v", ret)
	}

	// string
	r = bytes.NewReader([]byte("12:foobarraboof"))
	ret = data.ParseBencoded(r)
	if ret != 42 {
		t.Errorf("expected [42], got %v", ret)
	}

	// list with one int
	r = bytes.NewReader([]byte("li42ee"))
	ret = data.ParseBencoded(r)
	if ret != 42 {
		t.Errorf("expected [42], got %v", ret)
	}

	// list with one item
	r = bytes.NewReader([]byte("li42ee"))
	ret = data.ParseBencoded(r)
	if ret != 42 {
		t.Errorf("expected [42], got %v", ret)
	}
}
