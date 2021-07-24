class Solution:
    def uniquePaths(self, m: int, n: int) -> int:
        if m == 1 or n == 1:
            return 1
        # we only need to keep track of the previous row
        # and we can update as we go
        arr = [1]*n
        for row in range(1, m):
            for col in range(1, n):
                arr[col] = arr[col-1]+arr[col]
        return arr[-1]