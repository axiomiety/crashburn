# The guess API is already defined for you.
# @param num, your guess
# @return -1 if my number is lower, 1 if my number is higher, otherwise return 0
# def guess(num: int) -> int:

class Solution:
    def guessNumber(self, n: int) -> int:
        start, end = 1, 2**31-1
        while start <= end:
            mid = (start+end)//2
            val = guess(mid)
            if val == 0:
                return mid
            if val < 0:
                end = mid-1
            else:
                start = mid+1
        return start