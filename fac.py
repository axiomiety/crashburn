from timeit import default_timer
import numpy
from time import sleep

# from Corey Goldberg
# though the context manager in http://stackoverflow.com/questions/2327719/timing-block-of-code-in-python-without-putting-it-in-a-function looks nice
class Timer(object):
    def __init__(self, verbose=False):
        self.verbose = verbose
        self.timer = default_timer

    def __enter__(self):
        self.start = self.timer()
        return self

    def __exit__(self, *args):
        end = self.timer()
        self.elapsed_secs = end - self.start
        self.elapsed = self.elapsed_secs * 1000  # millisecs
        if self.verbose:
            print('elapsed time: %f s' % self.elapsed_secs)

# 10 digits factors
p1=4093082899
p2=5463458053

# 20 digits number
c=p1*p2

import math

# Newton's method
def isqrt(n):
    x = n
    y = (x + 1) // 2
    while y < x:
        x = y
        y = (x + n // x) // 2
    return x

def is_integer_square(n):
  v = isqrt(n)
  return v*v == n

import gmpy2

def fermat1(c):
  a = math.ceil(math.sqrt(c))
  b2 = a*a - c
  count = 0
  v = gmpy2.isqrt(b2)
  while v*v != b2:
    a = a + 1
    b2 = a*a - c
    #if count % 1000000 == 0:
    #  print('a {0}, b2 {1}, count {2}'.format(a,b2,count))
    #count += 1
    v = gmpy2.isqrt(b2) 

  ret = a - math.sqrt(b2)
  print('found factor {0}'.format(ret))
  return ret

def doit():
  with Timer(verbose=True):
    fermat1(c)

def q_x(x,n):
  return x**2 - n

def get_factor_base(n, B=4):
  # note that gmpy2.is_prime is *probabilistic*
  primes = [p for p in range(3,50) if gmpy2.is_prime(p)]
  fac_base = [2] + [p for p in primes if gmpy2.jacobi(p,n) == 1]
  return fac_base[:B]
  
def is_factor_base_compatible(a, fac_base):
  ''' returns true iff a can be completely factorised by the primes in fac_base'''
  factorisation = [0]*len(fac_base)
  for ind, p in enumerate(fac_base):
    while a%p == 0: # p is a factor of a
      factorisation[ind] +=1
      a = gmpy2.divexact(a,p)
      if a == 1:
        return (True, factorisation)
  return (False, None)

def find_interesting_q_x(n, fac_base):
  x = gmpy2.isqrt(n) # that's our starting point
  interesting_q_x = []
  # we have a fac_base of length B and need at worst B+1 Q(x)'s
  b_plus_1 = len(fac_base) + 1
  while len(interesting_q_x) < b_plus_1:
    suitable, factorisation = is_factor_base_compatible(q_x(x,n), fac_base)
    if suitable:
      interesting_q_x.append((x, q_x(x,n), factorisation))
    x+=1
  from pprint import pprint
  pprint(interesting_q_x)
  return interesting_q_x

def reduce_and_solve(vecs):
  mod_2_vectors = []
  for (_,_,f) in vecs:
    arr = numpy.array(f)
    mod_2_vectors.append(arr%2)

  A = numpy.matrix(mod_2_vectors)
  A_t = numpy.transpose(A)
  # we now want to solve for A_t*v=0
  return A

def row_reduction_in_gf2(m):
  A = numpy.matrix.copy(m)
  num_rows, num_cols = A.shape
  for j in range(num_cols):
    # find the first Aij equal to 1
    for i in range(num_rows):
      if A[i,j] == 1:
        print('found Aij: {0},{1}'.format(i+1,j+1))
        for k in range(num_cols):
          if k == j:
            continue # don't do anything!
          if A[i,k] == 1:
          # add col j to col k
            print('adding col {0} to col {1}'.format(j+1,k+1))
            A[:,k] = (A[:,k] + A[:,j])%2
        break
  return A
