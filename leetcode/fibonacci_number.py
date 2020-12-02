def memoize(f):
    cache = {}
    def m(*args):
        n = args[1]
        if n not in cache:
            cache[n] = f(*args)
        return cache[n]
    return m

class Solution:
    
    @memoize
    def fib(self, n: int) -> int:
        if n == 0:
            return 0
        elif n == 1:
            return 1
        else:
            return self.fib(n-1) + self.fib(n-2)