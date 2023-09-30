package data_test

import (
	"fmt"
	"go-bt/data"
	"io"
	"net/http/httptest"
	"net/url"
	"os"
	"reflect"
	"testing"
)

func TestParseTrackerResponse(t *testing.T) {
	file, _ := os.Open("testdata/tracker.response.beencoded")
	defer file.Close()
	bodyBytes, _ := io.ReadAll(file)
	t.Logf("%v\n", data.ParseTrackerResponse(bodyBytes))
	trackerResponse := data.ParseTrackerResponse(bodyBytes)
	expectedNumSeeds, expectedNumLeeches := int64(1500), int64(31)
	if trackerResponse.Complete != expectedNumSeeds {
		t.Errorf("expected %d seeds, found %d", expectedNumSeeds, trackerResponse.Complete)
	}
	if trackerResponse.Incomplete != expectedNumLeeches {
		t.Errorf("expected %d leeches, found %d", expectedNumLeeches, trackerResponse.Incomplete)
	}
	numExpectedPeers := 33
	if len(trackerResponse.Peers) != numExpectedPeers {
		t.Errorf("expected %d peers, found %d", numExpectedPeers, len(trackerResponse.Peers))
	}
}

func TestTrackerHandling(t *testing.T) {

	// dummy torrent
	torrent := data.ParseTorrentFile("testdata/ubuntu.torrent")
	// our tracker
	tracker := data.Tracker{
		InfoHashes:          map[[20]byte]data.TrackerResponse{},
		PeerLatestHeartBeat: map[string]int64{},
	}
	tracker.LoadTorrents("testdata")

	// args passed in to the tracker
	peerId := "12345678901234567890"
	port := 6882
	turl, _ := url.Parse("http://example.com/tracker")
	turl.RawQuery = fmt.Sprintf("info_hash=%s&peer_id=%s&port=%d&uploaded=0&downloaded=0&left=0", data.FormatInfoHash(torrent.InfoHash[:]), peerId, port)

	req := httptest.NewRequest("GET", turl.String(), nil)

	w := httptest.NewRecorder()

	tracker.TrackerQuery(w, req)

	resp := w.Result()
	body, _ := io.ReadAll(resp.Body)
	expected := []byte{}
	if !reflect.DeepEqual(body, expected) {
		//t.Errorf("response didn't match")
	}
}
