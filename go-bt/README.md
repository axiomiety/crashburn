
# Test harness

Whilst it's great fun to try this out against actual files, it's a lot easier to keep this on the local network (which has the advantage of enabling you to work on the implementation without needing an internet connection!).

The sample torrent file included can easily be modified with e.g. the port/address the tracker will be listening to.

To re-create the file:
```
mkfile 9m ~/tmp/file.out 
```

(choosing 9 so it's not a power of 2).

`file.torrent` contains the sample torrent file. The `md5sum` should be `b82b4ab87e44976024abc14a1670dac0`.