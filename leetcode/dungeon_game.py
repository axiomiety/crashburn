class Solution:
    def calculateMinimumHP(self, dungeon: List[List[int]]) -> int:
        mins = dungeon[-1][:]
        num_rows = len(dungeon)
        num_cols = len(mins)
        
        # this is our target
        mins[-1] = 1 if mins[-1] > 0 else abs(mins[-1])+1
        
        def calc(row, col):
            # dealing with the bottom right corner
            if col == num_cols-1 and row == num_rows - 1:
                return mins[col]
            # min for the handling the last column
            from_right = mins[min(col+1, num_cols-1)]
            # for handling the bottomw row
            from_below = from_right if row == num_rows - 1 else mins[col]
            sq_value = dungeon[row][col]
            lowest_required = min(from_right, from_below)
            if sq_value <= 0:
                return abs(sq_value) + lowest_required
            elif sq_value > lowest_required:
                return 1
            else:
                # if the 2 values are equal, we need at least 1!
                return max(abs(sq_value-lowest_required),1)
            
        
        for row in range(num_rows-1,-1,-1):
            for col in range(num_cols-1,-1,-1):
                mins[col] = calc(row, col)
              
        return mins[0]