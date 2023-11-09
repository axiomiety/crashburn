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

	// string, below 10 chars
	r = bytes.NewReader([]byte("3:foo"))
	ret = string(data.ParseBencoded(r).([]byte))
	if ret != "foo" {
		t.Errorf("expected 'foo', got %v", ret)
	}

	// string, above 10 chars
	r = bytes.NewReader([]byte("12:foobarraboof"))
	ret = string(data.ParseBencoded(r).([]byte))
	if ret != "foobarraboof" {
		t.Errorf("expected 'foo', got %v", ret)
	}

	// list with one int
	r = bytes.NewReader([]byte("li42ee"))
	retSlice, _ := data.ParseBencoded(r).([]interface{})
	if len(retSlice) != 1 && retSlice[0] != 42 {
		t.Errorf("expected [42], got %v", ret)
	}

	// list with two items
	r = bytes.NewReader([]byte("li42ei43ee"))
	retSlice, _ = data.ParseBencoded(r).([]interface{})
	if len(retSlice) != 2 && retSlice[0] != 42 && retSlice[1] != 43 {
		t.Errorf("expected [42], got %v", ret)
	}

	// a map
	r = bytes.NewReader([]byte("d3:fooi42ee"))
	retMap, _ := data.ParseBencoded(r).(map[string]interface{})
	if retMap["foo"] != 42 {
		t.Errorf("expected [42], got %v", retMap)
	}
}
