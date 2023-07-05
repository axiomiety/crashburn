package data_test

import (
	"testing"
	"go-bt/data"
	"os"
	"io"
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
	numExpectedPeers := 43
	if len(trackerResponse.Peers) != numExpectedPeers {
		t.Errorf("expected %d peers, found %d", len(trackerResponse.Peers), numExpectedPeers)
	}
}

