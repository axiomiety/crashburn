
class Entry:
    def __init__(self, before, after):
        self.before = before
        self.after = after

    def __repr__(self):
        return f"Entry({self.before} <-> {self.after})"

class LRU:
    def __init__(self, maxCapacity):
        self.maxCapacity  = maxCapacity
        self.currentCapacity = 0
        self.data = {}
        self.ordering = {}
        self.head = None
        self.tail = None
    
    def put(self, key, val):
        if key in self.data:
            self.data[key] = val
            self._restack(key)
        else:
            # new key!
            # make space if we're at capacity
            if self.currentCapacity == self.maxCapacity:
                self._evict()
            self.data[key] = val
            newHead = Entry(self.head, None)
            self.ordering[key] = newHead
            if self.head:
                currHead = self.ordering[self.head]
                currHead.after = key
            if not self.tail:
                self.tail = key
            self.head = key
            self.currentCapacity += 1
    
    def get(self, key):
        if key in self.data:
            self._restack(key)
            return self.data[key]
        else:
            return None # intentional

    def _evict(self):
        # remove the tail from the cache
        del self.data[self.tail]
        # find the entry the tail was pointing to
        # this is our new tail
        entry = self.ordering[self.tail]
        del self.ordering[self.tail]
        self.tail = entry.after
        entry.before = None
        self.currentCapacity -= 1

    def _restack(self, key):
        if key == self.head:
            pass # nothing to do
        else:
            entry = self.ordering[key]
            entryBefore = self.ordering[entry.before]
            entryBefore.after = entry.after
            # move to the top
            headEntry = self.ordering[self.head]
            headEntry.after = key
            entry.before = self.head
            self.head = key
            entry.after = None

    def _walk(self):
        ret = []
        k = self.tail
        while self.ordering[k].after != None:
            ret.append(k)
            k = self.ordering[k].after
        if self.tail != self.head:
            ret.append(k)
        return ret

def test1():
    cache = LRU(maxCapacity=3)
    cache.put('a',1)
    cache.put('b',1)
    cache.put('c',1)
    assert cache._walk() == ['a','b','c']
    assert len(cache.ordering) == 3
    assert len(cache.data) == 3
    cache.put('d',1)
    assert cache._walk() == ['b','c','d']
    assert len(cache.ordering) == 3
    assert len(cache.data) == 3
    cache.put('c',2)
    assert cache.get('c') == 2
    assert cache._walk() == ['b','d','c']
    assert len(cache.ordering) == 3
    assert len(cache.data) == 3
    cache.get('c')
    assert cache._walk() == ['b','d','c']
    assert len(cache.ordering) == 3
    assert len(cache.data) == 3

if __name__ == "__main__":
    test1()