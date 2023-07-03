package data

import (
	"bytes"
	"github.com/marksamman/bencode"
	"log"
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
	Peers []Peer
}

type Peer struct {
	Id string
	IP string
	Port int64
}

func parsePeer(dict map[string]any) Peer {
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
	return peer
}

func parsePeers(peersList []interface{}) []Peer {
	peers := make([]Peer, 10)
	for _, item := range(peersList) {
		//fmt.Printf("%v\n", parsePeer(item.(map[string]any)))
		peers = append(peers, parsePeer(item.(map[string]any)))
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
