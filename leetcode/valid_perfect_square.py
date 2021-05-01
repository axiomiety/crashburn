class Solution:
    def isPerfectSquare(self, num: int) -> bool:
        if num == 1:
            return True
        start, end = 1, num // 2
        while start <= end:
            mid = start + (end-start)//2
            sq = mid*mid
            if sq == num:
                return True
            elif sq > num:
                end = mid-1
            else:
                start = mid+1
        return False