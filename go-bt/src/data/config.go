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

		"create": {
			"filename": "filename.only",
			"directory": "where/the/file/is/located"
		}
	}

	ideally the first 3 would be a list of torrents - to support more than one at a time
*/
type Configuration struct {
	Torrent      string     `json:"torrent"`
	PiecesPath   string     `json:"piecesPath"`
	OutPath      string     `json:"outPath"`
	PeerId       string     `json:"peerId"`
	MaxPeers     uint8      `json:"maxPeers"`
	TorrentsPath string     `json:"torrentsPath"`
	Create       CreateConf `json:"create"`
}

type CreateConf struct {
	Filename  string `json:"filename"`
	Directory string `json:"directory"`
}
