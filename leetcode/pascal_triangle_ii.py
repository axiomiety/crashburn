from functools import lru_cache
from typing import List

class Solution:    
    def getRow(self, rowIndex: int) -> List[int]:
        
        # mmm... is that cheating?
        # the alternative is to kick off with a fixed-size array
        # and start computing from the root
        @lru_cache
        def relation(i, j):
            if j == 1 or i == j:
                return 1
            else:
                return relation(i-1,j-1) + relation(i-1,j)
        
        return [relation(rowIndex+1,j+1) for j in range(0,rowIndex+1)]

# another solution that builds the triangle from the ground up
def doit(row_idx):
    cache = [0 for _ in range((row_idx+1)*(row_idx+2)//2+1)]
    def relation(i, j):
        _idx = (i-1)*i//2+j
        if not cache[_idx]:
            if j == 1 or i == j:
                cache[_idx] = 1
            else:
                cache[_idx] = relation(i-1,j-1) + relation(i-1,j)
        return cache[_idx]

    for r in range(row_idx+1):
        for j in range(r+1):
            relation(r+1,j+1)

    return cache[-row_idx-1:]