import math
class Solution:
    def kthGrammar(self, N: int, K: int) -> int:
        if N == 1:
            return 0
        elif N == 2:
            return 0 if K == 1 else 1
        else:
            parent = int(math.ceil(K/2.0))
            parent_dash = 0 if K%2 == 1 else 1
            if self.kthGrammar(N-1,parent) == 0:
                return (0,1)[parent_dash]
            else:
                return (1,0)[parent_dash]