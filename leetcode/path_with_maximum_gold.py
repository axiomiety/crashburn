class Solution:
    def getMaximumGold(self, grid: List[List[int]]) -> int:
        num_rows, num_cols = len(grid), len(grid[0])
        max_gold = 0
        
        def get_value(v):
            return grid[v[0]][v[1]]
        
        def backtrack(total, seen, pos):
            nonlocal max_gold
            if total > max_gold:
                max_gold = total
            row, col = pos
            up = row-1, col
            down = row+1, col
            left = row, col-1
            right = row, col+1
            if 0 <= up[0] and up not in seen and get_value(up):
                new_seen = seen.copy()
                new_seen.add(up)
                backtrack(total+get_value(up), new_seen, up)
            if down[0] < num_rows and down not in seen and get_value(down):
                new_seen = seen.copy()
                new_seen.add(down)
                backtrack(total+get_value(down), new_seen , down)
            if left[1] >= 0 and left not in seen and get_value(left):
                new_seen = seen.copy()
                new_seen.add(left)
                backtrack(total+get_value(left), new_seen, left)
            if right[1] < num_cols and right not in seen and get_value(right):
                new_seen = seen.copy()
                new_seen.add(right)
                backtrack(total+get_value(right), new_seen, right)
        
        for r in range(num_rows):
            for c in range(num_cols):
                if get_value((r,c)):
                    backtrack(get_value((r,c)), {(r,c)}, (r,c))
        return max_gold