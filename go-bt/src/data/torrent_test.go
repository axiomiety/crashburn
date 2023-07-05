package data_test

import (
	"testing"
	"go-bt/data"
	"reflect"
)

func TestParseTorrentFile(t *testing.T) {
	torrent := data.ParseTorrentFile("testdata/ubuntu.torrent")

	expectedTorrentName := "ubuntu-22.04.2-live-server-amd64.iso"
	if torrent.Info.Name != expectedTorrentName {
		t.Errorf("info.name is expected to be %s, got %s", expectedTorrentName, torrent.Info.Name)
	}
	expectedAnnounce := "https://torrent.ubuntu.com/announce"
	if torrent.Announce != expectedAnnounce {
		t.Errorf("announce is expected to be %s, got %s", expectedAnnounce, torrent.Announce)
	}
	expectedAnnounceList := []string{"https://torrent.ubuntu.com/announce","https://ipv6.torrent.ubuntu.com/announce"}
	if !reflect.DeepEqual(torrent.AnnounceList, expectedAnnounceList) {
		t.Errorf("announce-list is expected to be %v, got %v", expectedAnnounceList, torrent.AnnounceList)
	}
}

