from functools import lru_cache

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