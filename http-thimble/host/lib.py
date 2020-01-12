from collections import defaultdict
import base64

class InMemoryStore(object):
    
    def __init__(self):
        self.store = defaultdict(list)

    def add(self, file_key: str, idx: int, chunk: str):
        # we don't know how many chunks we may receive, and we don't
        # really need to know until someone requests for it
        self.store[file_key].append( (idx, base64.urlsafe_b64decode(chunk.encode()) )