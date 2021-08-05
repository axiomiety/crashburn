class Solution:
    def uniquePathsWithObstacles(self, obstacleGrid: List[List[int]]) -> int:
        m = len(obstacleGrid)
        n = len(obstacleGrid[0])
        
        # we only need to keep track of the previous row
        # and we can update as we go
        arr = [0]*n
        for idx, c in enumerate(obstacleGrid[0]):
            if c == 0:
                arr[idx] = 1
            else:
                # can't proceed any further
                break
        
        for row in range(1,m):
            # need to reset the first col if it's an obstacle
            # as it's unreachable
            if obstacleGrid[row][0] == 1:
                arr[0] = 0
            for col in range(1,n):
                if obstacleGrid[row][col] == 0:
                    arr[col] = arr[col-1]+arr[col]
                else:
                    arr[col] = 0
        return 0 if obstacleGrid[m-1][n-1] else arr[-1]