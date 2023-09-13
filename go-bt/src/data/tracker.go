package data

import (
	"bytes"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"io/ioutil"
	"log"
	"net/http"
	"net/url"
	"strconv"
	"strings"

	"github.com/marksamman/bencode"
)

/*
failure reason: If present, then no other keys may be present. The value is a human-readable error message as to why the request failed (string).
warning message: (new, optional) Similar to failure reason, but the response still gets processed normally. The warning message is shown just like an error.
interval: Interval in seconds that the client should wait between sending regular requests to the tracker
min interval: (optional) Minimum announce interval. If present clients must not reannounce more frequently than this.
tracker id: A string that the client should send back on its next announcements. If absent and a previous announce sent a tracker id, do not discard the old value; keep using it.
complete: number of peers with the entire file, i.e. seeders (integer)
incomplete: number of non-seeder peers, aka "leechers" (integer)
peers: (dictionary model) The value is a list of dictionaries, each with the following keys:
	peer id: peer's self-selected ID, as described above for the tracker request (string)
	ip: peer's IP address either IPv6 (hexed) or IPv4 (dotted quad) or DNS name (string)
	port: peer's port number (integer)
peers: (binary model) Instead of using the dictionary model described above, the peers value may be a string consisting of multiples of 6 bytes. First 4 bytes are the IP address and last 2 bytes are the port number. All in network (big endian) notation.

*/

type TrackerResponse struct {
	Complete   int64 // seeds
	Incomplete int64 // leechers
	Interval   int64 // in seconds
	Peers      []Peer
}

type Peer struct {
	Id   string
	IP   string
	Port int64
}

func parsePeer(dict map[string]any) (Peer, error) {
	peer := Peer{}
	for key, value := range dict {
		switch key {
		case "peer id":
			peer.Id = value.(string)
		case "ip":
			peer.IP = value.(string)
		case "port":
			if val, ok := value.(int64); ok {
				peer.Port = val
			}
		default:
			log.Printf("ignoring key %s", key)
		}
	}
	var err error
	if peer.IP == "" || peer.Port == 0 {
		err = errors.New(fmt.Sprintf("invalid peer: IP=%s, port=%d", peer.IP, peer.Port))
	}
	return peer, err
}

func parsePeers(peersList []interface{}) []Peer {
	var peers []Peer
	for _, item := range peersList {
		peer, err := parsePeer(item.(map[string]any))
		if err != nil {
			log.Printf("bad peer: %s\n", err)
			continue
		}
		peers = append(peers, peer)
	}
	return peers
}

func ParseTrackerResponse(resp []byte) TrackerResponse {
	reader := bytes.NewReader(resp)
	dict, err := bencode.Decode(reader)
	check(err)

	trackerResponse := TrackerResponse{}
	for key, value := range dict {
		switch key {
		case "complete":
			if val, ok := value.(int64); ok {
				trackerResponse.Complete = val
			}
		case "incomplete":
			if val, ok := value.(int64); ok {
				trackerResponse.Incomplete = val
			}
		case "interval":
			if val, ok := value.(int64); ok {
				trackerResponse.Interval = val
			}
		case "peers":
			trackerResponse.Peers = parsePeers(value.([]interface{}))
		default:
			log.Printf("ignoring key %s", key)
		}
	}

	return trackerResponse
}

func formatInfoHash(hash []byte) string {
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

func (torrent *Torrent) QueryTracker() TrackerResponse {
	turl, err := url.Parse(torrent.Announce)
	check(err)
	peerId := "12345678901234567890"
	port := 6882
	turl.RawQuery = fmt.Sprintf("info_hash=%s&peer_id=%s&port=%d&uploaded=0&downloaded=0&left=0", formatInfoHash(torrent.InfoHash[:]), peerId, port)
	resp, err := http.Get(turl.String())
	check(err)
	defer resp.Body.Close()
	bodyBytes, err := io.ReadAll(resp.Body)
	check(err)
	return ParseTrackerResponse(bodyBytes)
}

type Tracker struct {
	InfoHashes map[[20]byte]TrackerResponse
}

func (tracker *Tracker) list(w http.ResponseWriter, req *http.Request) {
	w.Header().Add("Content-Type", "application/text")
	for key, val := range tracker.InfoHashes {
		io.WriteString(w, fmt.Sprintf("%s:%v\n", key, val))
	}
}
func (tracker *Tracker) trackerQuery(w http.ResponseWriter, req *http.Request) {
	query := req.URL.Query()
	infoHash := [20]byte{}
	copy(infoHash[:], query.Get("info_hash"))
	if _, ok := tracker.InfoHashes[infoHash]; ok {
		peerId := query.Get("peer_id")
		peerPortStr := query.Get("port")
		// maybe look at X-FORWARDED-FOR?
		log.Printf("got request from %s:%s - %v", req.RemoteAddr, peerPortStr, peerId)
		// add the Peer to the list
		peer := Peer{
			Id: peerId,
			IP: req.RemoteAddr,
		}
		if peerPort, err := strconv.ParseInt(peerPortStr, 10, 64); err != nil {
			peer.Port = peerPort
		}
		// we likely need to de-dupe peers - maybe this isn't the right type!
		m := tracker.InfoHashes[infoHash]
		m.Peers = append(tracker.InfoHashes[infoHash].Peers, peer)

	}
}

func encodeTrackerResponse(resp TrackerResponse) []byte {
	m := map[string]any{}
	m["complete"] = resp.Complete
	m["incomplete"] = resp.Incomplete
	m["interval"] = resp.Interval
	peers := [][]byte{}
	for _, peer := range resp.Peers {
		peerMap := map[string]any{}
		peerMap["peer id"] = peer.Id
		peerMap["ip"] = peer.IP
		peerMap["port"] = peer.Port
	}
	m["peers"] = peers
	return bencode.Encode(m)
}

func (tracker *Tracker) loadTorrents(path string) {
	files, err := ioutil.ReadDir(path)
	if err != nil {
		panic(err)
	}

	for _, filename := range files {
		if strings.HasSuffix(filename.Name(), ".torrent") {
			log.Printf("torrent file found: %s\n", filename.Name())
			torrent := ParseTorrentFile(filename.Name())
			tracker.InfoHashes[torrent.InfoHash] = TrackerResponse{
				Complete:   0,
				Incomplete: 0,
				Peers:      make([]Peer, 5),
				Interval:   30,
			}
		}
	}

}

func (tracker *Tracker) Serve(port int, torrentsPath string) {
	if tracker.InfoHashes == nil {
		tracker.InfoHashes = map[[20]byte]TrackerResponse{}
	}
	tracker.loadTorrents(torrentsPath)
	http.HandleFunc("/list", tracker.list)
	http.HandleFunc("/tracker", tracker.trackerQuery)
	http.ListenAndServe(fmt.Sprintf(":%d", port), nil)
}
