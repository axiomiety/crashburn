package main

import (
	"go-bt/data"
	"os"
	"io"
	"fmt"
)

func main() {
	fmt.Printf("%v\n", data.ParseTorrentFile("ubuntu-22.04.2-live-server-amd64.iso.torrent"))

	file, _ := os.Open("tracker.response.beencoded")
	defer file.Close()
	bodyBytes, _ := io.ReadAll(file)
	fmt.Printf("%v\n", data.ParseTrackerResponse(bodyBytes))
}