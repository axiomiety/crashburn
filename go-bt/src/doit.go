package main

import (
	"go-bt/data"
	"fmt"
)

func main() {
	fmt.Printf("%v\n", data.ParseTorrentFile("ubuntu-22.04.2-live-server-amd64.iso.torrent"))
}