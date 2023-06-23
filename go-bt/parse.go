package main

import (
	"fmt"
	"os"
	"github.com/marksamman/bencode"
)

func main() {
	fname := "ubuntu-22.04.2-live-server-amd64.iso.torrent"
	file, err := os.Open(fname)
	defer file.Close()
	if err != nil {
		panic(err)
	}
	
	dict, err := bencode.Decode(file)
	if err != nil {
		panic(err)
	}
	for key, _ := range(dict) {
		fmt.Println(key)
	}
	// sadly, 'cause the expression is of type interface{}
	fmt.Println(dict["info"].(map[string]interface{})["name"])
	fmt.Println(dict["announce"])
}