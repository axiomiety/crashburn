class Solution:
    def generateMatrix(self, n: int) -> List[List[int]]:
        from itertools import cycle
            
        # fill in the grid with 0's
        grid = []
        for _ in range(n):
            row = []
            for _ in range(n):
                row.append(0)
            grid.append(row)
        
        
         # right, down, left up in row, col indexing
        directions = cycle([(0,1),(1,0),(0,-1),(-1,0)])
        direction = next(directions)
        
        def is_valid(r, c):
            return 0 <= r < n and 0<= c < n and grid[r][c] == 0
        
        def move(pos):
            nonlocal direction
            new_pos = pos[0] + direction[0], pos[1] + direction[1]
            while not is_valid(*new_pos):
                direction = next(directions)
                new_pos = pos[0] + direction[0], pos[1] + direction[1]
            return new_pos
        
        r,c = 0,0
        grid[r][c] = 1
        for elem in range(2,n**2+1):
            r, c = move((r,c))
            grid[r][c] = elem
        return grid