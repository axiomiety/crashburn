import base64
from collections import Counter

def cycle_key(key):
    key = key if isinstance(key, list) else [key]
    idx = 0
    while True:
      yield key[idx%len(key)]
      idx += 1

def readf(fname):
    with open(fname, 'rU') as f:
        lines = f.readlines()
    
    return base64.b64decode(''.join(l.strip() for l in lines))

def hamming_dist_single_byte(a,b):
    xored = a^b
    return sum((xored >> i) & 1 for i in range(8))

def hamming_dist(a,b):
    return sum(hamming_dist_single_byte(aa,bb) for aa,bb in zip(a,b))

def score(chunk,k):
    ''' if the key is right, the char distribution of each chunk
        should roughly reflect what we should see in the input language
    '''
    xored_chunk = bytes([a^b for (a,b) in zip(chunk, cycle_key(k))])
    freqs = Counter(xored_chunk)
    mcs = freqs.most_common()[:12]
    s = 'etaoin shrdlu'

    mm = set(x for (x,_) in mcs)
    score = len(set(bytes(s, 'ascii')).intersection(mm)) + \
            len(set(bytes(s.upper(), 'ascii')).intersection(mm))
    return score

def get_most_probably_single_char_key(chunk):
    ''' we don't limit ourselves to printable ascii '''
    best_k, best_score = 0, 0
    for k in range(256): # all possible bit values in a single byte
        key_score = score(chunk, k)
        if key_score > best_score:
            best_score = key_score
            best_k = k
    return best_k

def get_chunks(o,chunk_size=2):
    chunks = zip(*[iter(o)]*chunk_size)
    return [bytes(c) for c in chunks]

def get_avg_hamming_dist(raw, chunk_size):
    chunks = get_chunks(raw, chunk_size)
    return sum(hamming_dist(c1,c2)/chunk_size for c1,c2 in zip(chunks,chunks[1:]))/len(chunks[1:])

def potential_key_lengths(data):
    ''' return the 10 key lengths with lowest hamming distances '''
    dists = []
    for key_length in range(2,20):
        dists.append( (key_length, get_avg_hamming_dist(data, key_length)) )
    dists = sorted(dists, key=lambda x: x[1])
    return dists

def challenge():
    raw = readf('c.txt')
    for n, _ in potential_key_lengths(raw):
        transposed_chunks = [raw[i::n] for i in range(n)]
        key = []
        for idx, chunk in enumerate(transposed_chunks):
            k = get_most_probably_single_char_key(chunk)
            key.append(k)

        print(''.join(chr(k) for k in key))
        print(bytes([a^b for (a,b) in zip(raw, cycle_key(key))]))
