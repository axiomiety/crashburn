package data

/*
We expect a configuration file to be passed in of the following format:

	{
		"torrent": "path_to_torrent_file",
		"piecesPath": "path where the torrent file will be saved"
		"outPath": "path where the file(s) will be written to"
		"peerId": "20 bytes",
		"maxPeers": int, // max number of peers to connect to
		"torrentsPath": "/all/your/torrents/are/belong/to/us" - optional
	}
*/
type Configuration struct {
	Torrent      string `json:"torrent"`
	PiecesPath   string `json:"piecesPath"`
	OutPath      string `json:"outPath"`
	PeerId       string `json:"peerId"`
	MaxPeers     uint8  `json:"maxPeers"`
	TorrentsPath string `json:"torrentsPath"`
}
