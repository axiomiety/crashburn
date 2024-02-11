package data_test

import (
	"bytes"
	"go-bt/data"
	"io"
	"os"
	"testing"
)

func TestBencodeParsing(t *testing.T) {

	testCases := []struct {
		f func(io.Reader) interface{}
	}{
		{data.ParseBencoded},
		{data.ParseBencoded2},
	}
	for _, tc := range testCases {
		// single integer
		r := bytes.NewReader([]byte("i42e"))
		ret := tc.f(r)
		if ret != 42 {
			t.Errorf("expected 42, got %v", ret)
		}

		// string, below 10 chars
		r = bytes.NewReader([]byte("3:foo"))
		ret = string(tc.f(r).([]byte))
		if ret != "foo" {
			t.Errorf("expected 'foo', got %v", ret)
		}

		// string, above 10 chars
		r = bytes.NewReader([]byte("12:foobarraboof"))
		ret = string(tc.f(r).([]byte))
		if ret != "foobarraboof" {
			t.Errorf("expected 'foo', got %v", ret)
		}

		// list with one int
		r = bytes.NewReader([]byte("li42ee"))
		retSlice, _ := tc.f(r).([]interface{})
		if len(retSlice) != 1 && retSlice[0] != 42 {
			t.Errorf("expected [42], got %v", ret)
		}

		// list with two items
		r = bytes.NewReader([]byte("li42ei43ee"))
		retSlice, _ = tc.f(r).([]interface{})
		if len(retSlice) != 2 && retSlice[0] != 42 && retSlice[1] != 43 {
			t.Errorf("expected [42, 43], got %v", ret)
		}

		// a simple map
		r = bytes.NewReader([]byte("d3:fooi42ee"))
		retMap, _ := tc.f(r).(map[string]interface{})
		if retMap["foo"] != 42 {
			t.Errorf("expected [42], got %v", retMap)
		}

		// a map with a list
		r = bytes.NewReader([]byte("d3:fooli42eee"))
		retMap, _ = tc.f(r).(map[string]interface{})
		retSlice = retMap["foo"].([]interface{})
		if len(retSlice) != 1 && retSlice[0] != 42 {
			t.Errorf("expected {'foo': [42]}, got %v", ret)
		}
	}
}

func TestBencodeEncode(t *testing.T) {
	var b bytes.Buffer
	data.Encode(&b, 42)
	expected := []byte("i42e")
	if bb := b.Bytes(); !bytes.Equal(bb, expected) {
		t.Errorf("expected %v, got %v", bb, expected)
	}
}
func TestBencodeStructTags(t *testing.T) {
	file, _ := os.Open("testdata/ubuntu.torrent")
	btorrent := data.ParseTorrentFile2(file)

	expectedName := "ubuntu-22.04.2-live-server-amd64.iso"
	if btorrent.Info.Name != expectedName {
		t.Errorf("expected %s, found %s", expectedName, btorrent.Info.Name)
	}
	if btorrent.Info.Length != 1975971840 {
		t.Errorf("expected %d, found %d", 1975971840, btorrent.Info.Length)
	}
}
