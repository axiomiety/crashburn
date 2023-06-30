package main

import (
	"crypto/sha1"
	"encoding/hex"
	"fmt"
	"github.com/marksamman/bencode"
	// "go-bt/data"
	"io"
	"net/http"
	"net/url"
	"os"
	"strings"
	//"strconv"
)

func format_hash(hash []byte) string {
	// look at https://stackoverflow.com/questions/25192805/encoding-info-hash-for-request-torrent-tracker for inspiration
	var sb strings.Builder
	for idx, val := range hash {
		if val <= 127 {
			new_val := url.QueryEscape(string(val))
			if string(new_val[0]) == "%" {
				new_val = strings.ToLower(new_val)
			}
			sb.WriteString(new_val)
		} else {
			sb.WriteString(fmt.Sprintf("%%%s", hex.EncodeToString(hash[idx:idx+1])))
		}
	}
	return sb.String()
}

func tracker_get(info_hash [20]byte, tracker_url string) {
	turl, _ := url.Parse(tracker_url)
	turl.RawQuery = fmt.Sprintf("info_hash=%s&peer_id=%s&port=%s&uploaded=0&downloaded=0&left=0", format_hash(info_hash[:]), "12345678901234567890", "6882") // params.Encode()

	fmt.Printf("%#v\n", turl)
	fmt.Println("wah")
	resp, err := http.Get(turl.String())
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()
	bodyBytes, err := io.ReadAll(resp.Body)
	f, _ := os.Create("tracker.response.beencoded")
	f.Write(bodyBytes)
	defer f.Close()
	fmt.Println(string(bodyBytes))
}

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
	for key, _ := range dict {
		fmt.Println(key)
	}
	// announceList, _ := dict["announce-list"].([]interface{})
	// torrent := data.Torrent{
	// 	Announce: dict["announce"].(string),
	// 	AnnounceList: announceList,
	// }
	// fmt.Println("%v", torrent)
	// sadly, 'cause the expression is of type interface{}
	info := dict["info"].(map[string]interface{})
	for k, v := range info {
		if k == "pieces" { // it's binary data - sigh
			continue
		}
		fmt.Printf("%s:%v\n", k, v)
	}
	fmt.Println(dict["announce"])
	fmt.Println(dict["announce-list"])

	tracker_get(sha1.Sum(bencode.Encode(dict["info"])), dict["announce"].(string))
	test_hash, _ := hex.DecodeString("7cd350e5a70f0a61593e636543f9fc670ffa8a4d")
	fmt.Println(format_hash(test_hash[:]))
	fmt.Println("%7c%d3P%e5%a7%0f%0aaY%3eceC%f9%fcg%0f%fa%8aM")
}
