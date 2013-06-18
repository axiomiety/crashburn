'''
Originally written to help me complete the Matasano Crypto challenge
This collection of utils is quite aggressive - most methods use asserts for sanity checks. They also pretty much operate exclusively on bytes.
Where possible, imports are done at the fn level so you don't need to install modules for stuff you won't use.

PREREQUISITES:
  python3-crypto
'''
import  binascii
import  random
import  unittest
from    collections import  Counter

def is_using_ECB(raw, blocksize):
  indexes = range(0,len(raw), blocksize)
  d = []
  for (start,end) in zip(indexes, indexes[1:]):
    d.append( raw[start:end] )
  cn = Counter(d)
  if cn.most_common()[0][1] > 1:
    return True
  else:
    return False

def generate_random_key(length=3):
  bar = bytearray()
  for i in range(length):
    bar.append( random.randint(0,255) )
  return bytes(bar)

def generate_random_AES_key():
  return generate_random_key(length=16) # well, can be 16, 24, 32

def b64_file_to_bytes(filename):

  with open(filename, 'rU') as f:
    lines = f.readlines()

  data = ''.join([l.strip() for l in lines])
  return bytes(binascii.a2b_base64(data.encode('ascii')))

def pad2(b, blocksize=16):
  ''' a PKCS#7 padding implementation '''
  pad_length = blocksize
  if len(b) % blocksize:
    pad_length = blocksize - (len(b) % blocksize)
  
  return b + bytes((pad_length,))*pad_length

def unpad2(b):
  pad_length = b[-1]
  return b[:-pad_length]

def pad(b, length):
  assert(isinstance(b, bytes))
  assert(len(b) <= length)

  pad_length = length - len(b)
  assert(pad_length < 256) # as we're padding with bytes
  if pad_length:
    padding = bytes((pad_length,))*pad_length
    return b + padding
  else:
    return b

def fill_key(k, txt):
  p = len(txt)//len(k)
  q = len(txt)%len(k)
  return k*p + k[:q]

def hamming_distance(p, q):
  assert( len(q) == len(p) )
  return sum([_hamdist_byte(a,b) for (a,b) in zip(p,q)])

def _hamdist_byte(b1, b2):
  dist = 0
  v = b1^b2
  while v:
    if (v & 1): # odd - so rightmost bit is set, hence that counts as a difference
      dist += 1
    v = v >> 1
  return dist

def slice(b, idx, jump):
  bar = bytearray()
  while idx < len(b):
    bar.append(b[idx])
    idx += jump
  return bytes(bar)

def get_blocks(data, size=16):
  assert( isinstance(data, bytes) )

  # make data a multiple of size bits
  if len(data)%size != 0:
    padded_len = size*(len(data)//size + 1)
    data = pad(data, padded_len)

  # divide data in size chunks
  num_blocks = len(data)//size

  blocks = []
  for i in range(0, num_blocks):
    st = i*size
    blocks.append(data[st:st+size])

  return blocks

def xor_encrypt(k, txt, pad=True):
  assert( isinstance(k, bytes) )
  assert( isinstance(txt, bytes) )
  kk = fill_key(k, txt) if pad else k
  return bytes([a^b for (a,b) in zip(kk, txt)])

def aes_encrypt_cbc(k, txt, iv):
  assert( isinstance(k, bytes) )
  assert( isinstance(txt, bytes) )
  assert( isinstance(iv, bytes) )
  assert( len(iv) == 16 )

  # for each chunk, xor with previous one
  #   -> encrypt
  from Crypto.Cipher import AES
  mode = AES.MODE_ECB
  #TODO: check k is either 16, 24 or 32 bytes long
  cr = AES.new(k, mode)

  prev_block = iv
  blocks = get_blocks(txt, size=16)
  enc = bytearray()
  for block in blocks:
    prev_block = cr.encrypt(xor_encrypt(prev_block, block, pad=False))
    enc.extend(prev_block)

  return bytes(enc)

def aes_decrypt_cbc(k, txt, iv):
  assert( isinstance(k, bytes) )
  assert( isinstance(txt, bytes) )
  assert( isinstance(iv, bytes) )
  assert( len(iv) == 16 )

  from Crypto.Cipher import AES
  mode = AES.MODE_ECB
  cr = AES.new(k, mode)

  prev_block = iv
  blocks = get_blocks(txt, size=16)
  dec = bytearray()
  for block in blocks:
    dec.extend(xor_encrypt(cr.decrypt(block), prev_block, pad=False))
    #prev_block = block
    prev_block = block

  return bytes(dec)

class AESTest(unittest.TestCase):

  def test_inverse(self):
    plaintext = b'this is a text with more than 16 bytes'
    key = b'yellow submarine'
    iv = pad(b'1', 16)
    cr = aes_encrypt_cbc(key, plaintext, iv)
    dec = aes_decrypt_cbc(key, cr, iv)
    self.assertEqual( dec[:len(plaintext)], plaintext )

class XORTest(unittest.TestCase):

  def test_xor_encrypt(self):
    text = b'Vanilla Ice Ice Baby!'
    randomkey = generate_random_key(length=random.randint(1,len(text)))
    s = xor_encrypt(randomkey, text)
    self.assertEqual( text, xor_encrypt(randomkey, s) )

class HelpersTest(unittest.TestCase):
  
  @classmethod
  def setUpClass(cls):
    cls.b1 = bytes('ice ice baby', 'ascii')

  def test_pad(self):
    #self.assertRaises(AssertionError, pad('abc',3))
    #self.assertRaises(AssertionError, pad('abc',2))
    self.assertEqual( self.b1, pad(self.b1, len(self.b1)) )
    self.assertEqual( self.b1 + bytes((1,)), pad(self.b1, len(self.b1)+1) )
    self.assertEqual( bytes('ice ice baby\x02\x02', 'ascii'), pad(self.b1, len(self.b1)+2) )

  def test_pad2(self):
    self.assertEqual( bytes('ice ice baby\x02\x02', 'ascii'), pad2(self.b1, len(self.b1)+2) )
    self.assertEqual( self.b1, unpad2(pad2(self.b1)))

  def test_fill_key(self):
    self.assertEqual( b'ice', fill_key(b'ice', b'ice') )
    self.assertEqual( b'aaa', fill_key(b'a', b'ice') )
    self.assertEqual( b'van', fill_key(b'vanilla', b'ice') )

  def test_hamming_distance(self):
    self.assertEqual( 37, hamming_distance(b'this is a test', b'wokka wokka!!!'))

  def test_get_blocks(self):
    data = b'abcdefghi'
    blocks = get_blocks(data, 4)
    self.assertEqual( [b'abcd', b'efgh', b'i\x03\x03\x03'], blocks )

if __name__ == "__main__":
    unittest.main()
