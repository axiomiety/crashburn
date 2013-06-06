'''
Originally written to help me complete the Matasano Crypto challenge
This collection of utils is quite aggressive - most methods use asserts for sanity checks. They also pretty much operate exclusively on bytes.
'''
import  random
import  unittest
#from  Crypto.Cipher import  AES

def pad(b, length):
  ''' a PKCS#7 padding implementation '''
  assert(isinstance(b, bytes))
  assert(len(b) <= length)
  assert(length < 256) # as we're padding with bytes

  pad_length = length - len(b)
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

def xor_encrypt(k, txt):
  kk = fill_key(k, txt)
  return bytes([a^b for (a,b) in zip(kk, txt)])

class XOREncryptionTest(unittest.TestCase):
  
  @staticmethod
  def _generate_random_key(length=3):
    bar = bytearray()
    for i in range(length):
      bar.append( random.randint(0,255) )
    return bytes(bar)

  def test_xor_encrypt(self):
    text = b'Vanilla Ice Ice Baby!'
    randomkey = XOREncryptionTest._generate_random_key(length=random.randint(1,len(text)))
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

  def test_fill_key(self):
    self.assertEqual( b'ice', fill_key(b'ice', b'ice') )
    self.assertEqual( b'aaa', fill_key(b'a', b'ice') )
    self.assertEqual( b'van', fill_key(b'vanilla', b'ice') )

  def test_hamming_distance(self):
    self.assertEqual( 37, hamming_distance(b'this is a test', b'wokka wokka!!!'))

if __name__ == "__main__":
    unittest.main()
