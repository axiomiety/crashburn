
# Test harness

Whilst it's great fun to try this out against actual files, it's a lot easier to keep this on the local network (which has the advantage of enabling you to work on the implementation without needing an internet connection!).

## Creating a `.torrent` for a dummy file

We start by creating a dummy file using the below, whose `md5sum` should be `b82b4ab87e44976024abc14a1670dac0`:
```
mkfile 9m ~/tmp/file.out 
```

To generate a `.torrent`, create a `config.json` containing the below:
```
{
    "create": {
        "filename": "file.out",
        "directory": "/path/to/file/"
    }
}
```
and call:
```
go run ./doit.go -c /path/to/config.json -n Create
```
(there are more dummy values hard-coded, change as you see fit).

## Spinning up the tracker

In a separate terminal:

```
go run ./doit.go -c /path/to/config.json -n Serve
```

You can then `curl http://localhost:8088/list` which should dump some basic info pertaining to the torrents loaded (if you're following the above, there should be a single entry):

```
5d574b18df13d561e1f84fd1304a18bcfed34e2b:{0 0 30 [{  0} {  0} {  0} {  0} {  0}]}
```

## Setting up the seed

We need a client that can seed the dummy file.

# TODO
 - de-dupe peers (if they're on the tracker but we already received an incoming connection)
 - client should serve blocks!
 - client should publish number of completed/missing blocks to tracker
 - need to manage choke/unchock better
 - client shouldn't die when a peer requests a block it doesn't have

# DONE
 - fix the hardcoding of ::1 everywhere...