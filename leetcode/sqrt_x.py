class Solution:
    def mySqrt(self, x: int) -> int:
        if x == 0:
            return 0
        start, end = 1, x//2
        while end > start:
            mid = (start+end)//2
            mid2 = mid**2
            if x == mid2:
                return mid
            elif x > mid2:
                start = mid+1
            else:
                end = mid-1
        return start-1 if start**2 > x else start