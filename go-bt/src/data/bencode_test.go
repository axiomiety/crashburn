package data_test

import (
	"bytes"
	"go-bt/data"
	"io"
	"os"
	"testing"
)

func TestBencodeDecode(t *testing.T) {

	testCases := []struct {
		data []byte
	}{
		{[]byte("i-42e")},
		{[]byte("3:foo")},
		{[]byte("12:foobarraboof")},
		{[]byte("li42ee")},
		{[]byte("li42ei43ee")},
		{[]byte("d3:fooi42ee")},
		{[]byte("d3:fooli42eee")},
		{[]byte("d3:fooi42e3:zari1ee")},
	}

	buf := &bytes.Buffer{}
	for _, testCase := range testCases {
		buf.Reset()
		data.Encode(buf, data.ParseBencoded2(bytes.NewReader(testCase.data)))
		if bytes.Compare(buf.Bytes(), testCase.data) != 0 {
			t.Errorf("expected %s, got %s", testCase.data, buf.Bytes())
		}
	}
}
func TestBencodeRecursiveParser(t *testing.T) {

	// negative int!
	r := bytes.NewReader([]byte("i-42e"))
	ret := data.ParseBencoded2(r)
	if ret != -42 {
		t.Errorf("expected -42, got %v", ret)
	}

	// string, below 10 chars
	r = bytes.NewReader([]byte("3:foo"))
	ret = data.ParseBencoded2(r).(string)
	if ret != "foo" {
		t.Errorf("expected 'foo', got %v", ret)
	}

	// string, above 10 chars
	r = bytes.NewReader([]byte("12:foobarraboof"))
	ret = data.ParseBencoded2(r).(string)
	if ret != "foobarraboof" {
		t.Errorf("expected 'foo', got %v", ret)
	}

	// list with one int
	r = bytes.NewReader([]byte("li42ee"))
	retSlice, _ := data.ParseBencoded2(r).([]any)
	if len(retSlice) != 1 && retSlice[0] != 42 {
		t.Errorf("expected [42], got %v", ret)
	}

	// list with two items
	r = bytes.NewReader([]byte("li42ei43ee"))
	retSlice, _ = data.ParseBencoded2(r).([]any)
	if len(retSlice) != 2 && retSlice[0] != 42 && retSlice[1] != 43 {
		t.Errorf("expected [42, 43], got %v", ret)
	}

	// a simple map
	r = bytes.NewReader([]byte("d3:foo3:bare"))
	// r = bytes.NewReader([]byte("d3:fooi42ee"))
	retMap, _ := data.ParseBencoded2(r).(map[string]any)
	if retMap["foo"] != "bar" {
		t.Errorf("expected {'foo': 'bar'}, got %v", retMap)
	}

	// a map with a list
	r = bytes.NewReader([]byte("d3:fooli42eee"))
	retMap, _ = data.ParseBencoded2(r).(map[string]any)
	retSlice = retMap["foo"].([]interface{})
	if len(retSlice) != 1 && retSlice[0] != 42 {
		t.Errorf("expected {'foo': [42]}, got %v", ret)
	}
}

func TestBencodeParsing(t *testing.T) {

	testCases := []struct {
		f func(io.Reader) interface{}
	}{
		// {data.ParseBencoded},
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
		ret = tc.f(r).(string)
		if ret != "foo" {
			t.Errorf("expected 'foo', got %v", ret)
		}

		// string, above 10 chars
		r = bytes.NewReader([]byte("12:foobarraboof"))
		ret = tc.f(r).(string)
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
	// int
	data.Encode(&b, 42)
	expected := []byte("i42e")
	if bb := b.Bytes(); !bytes.Equal(bb, expected) {
		t.Errorf("expected %v, got %v", expected, bb)
	}

	// string
	b.Reset()
	data.Encode(&b, "foobar")
	expected = []byte("6:foobar")
	if bb := b.Bytes(); !bytes.Equal(bb, expected) {
		t.Errorf("expected %s, got %s", string(expected), string(bb))

	}

	// list of ints
	b.Reset()
	data.Encode(&b, []int{1, 2, 3})
	expected = []byte("li1ei2ei3ee")
	if bb := b.Bytes(); !bytes.Equal(bb, expected) {
		t.Errorf("expected %v, got %v", expected, bb)
	}

	// ditto, but uint16
	b.Reset()
	data.Encode(&b, []uint16{1, 2, 3})
	expected = []byte("li1ei2ei3ee")
	if bb := b.Bytes(); !bytes.Equal(bb, expected) {
		t.Errorf("expected %v, got %v", expected, bb)
	}

	// list of strings
	b.Reset()
	data.Encode(&b, []string{"a", "bc", "def"})
	expected = []byte("l1:a2:bc3:defe")
	if bb := b.Bytes(); !bytes.Equal(bb, expected) {
		t.Errorf("expected %v, got %v", expected, bb)
	}

	// dictionary
	b.Reset()
	m := map[string]int{}
	m["def"] = 2
	m["abc"] = 1
	data.Encode(&b, m)
	// note the alphabetical order
	expected = []byte("d3:abci1e3:defi2ee")
	if bb := b.Bytes(); !bytes.Equal(bb, expected) {
		t.Errorf("expected %v, got %s", expected, bb)
	}

	// dictionary with nested list
	b.Reset()
	m2 := map[string]any{}
	m2["def"] = []int{1, 2, 3}
	m2["abc"] = "foo"
	data.Encode(&b, m2)
	expected = []byte("d3:abc3:foo3:defli1ei2ei3eee")
	if bb := b.Bytes(); !bytes.Equal(bb, expected) {
		t.Errorf("expected %v, got %s", expected, bb)
	}

	// floats are *not* supported!
	defer func() {
		if err := recover(); err == nil {
			t.Errorf("expected a panic!")
		}
	}()

	b.Reset()
	data.Encode(&b, 3.44)
}

func TestBencodeStructTags(t *testing.T) {
	file, _ := os.Open("testdata/ubuntu.torrent")
	defer file.Close()
	btorrent := data.ParseFromReader[data.BETorrent](file)

	expectedName := "ubuntu-22.04.2-live-server-amd64.iso"
	if btorrent.Info.Name != expectedName {
		t.Errorf("expected %s, found %s", expectedName, btorrent.Info.Name)
	}
	if btorrent.Info.Length != 1975971840 {
		t.Errorf("expected %d, found %d", 1975971840, btorrent.Info.Length)
	}
	// if !reflect.DeepEqual(btorrent.AnnounceList, []string{}) {
	// 	t.Errorf("%v", btorrent.AnnounceList)
	// }

	// do the same for a tracker response - plenty of nested structs
	file2, _ := os.Open("testdata/tracker.response.beencoded")
	defer file2.Close()
	trackerResponse := data.ParseFromReader[data.BETrackerResponse](file2)
	if len(trackerResponse.Peers) != 33 {
		t.Errorf("expected 2 peers, got %d", len(trackerResponse.Peers))
	}
}
