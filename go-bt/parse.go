package main

import (
	"fmt"
	"os"
	"github.com/marksamman/bencode"
	"net/http"
	"net/url"
	"encoding/hex"
	"crypto/sha1"
	"strings"
	"io"
	//"strconv"
)

func format_hash(hash []byte) string {
	// look at https://stackoverflow.com/questions/25192805/encoding-info-hash-for-request-torrent-tracker for inspiration
	var sb strings.Builder
	for idx, val := range(hash) {
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
	params := url.Values{}
	params.Add("info_hash", format_hash(info_hash[:]))
	params.Add("peer_id", "12345678901234567890")
	params.Add("port", "6882")
	turl.RawQuery = params.Encode()

	fmt.Printf("%#v\n",turl)
	fmt.Println("wah")
	resp, err := http.Get(turl.String())
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()
	bodyBytes, err := io.ReadAll(resp.Body)
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
	for key, _ := range(dict) {
		fmt.Println(key)
	}
	// sadly, 'cause the expression is of type interface{}
	info := dict["info"].(map[string]interface{})
	for k, v := range(info) {
		if k == "pieces" {  // it's binary data - sigh
			continue
		}
		fmt.Printf("%s:%v\n", k, v)
	}
	fmt.Println(dict["announce"])
	fmt.Println(dict["announce-list"])

	tracker_get(sha1.Sum(bencode.Encode(dict["info"])), dict["announce"].(string))
	test_hash, _ := hex.DecodeString("7cd350e5a70f0a61593e636543f9fc670ffa8a4d")
	fmt.Println(format_hash(test_hash[:]))
}