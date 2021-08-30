class Solution:
    def searchMatrix(self, matrix: List[List[int]], target: int) -> bool:
        num_rows = len(matrix)
        num_cols = len(matrix[0])
        num_elems = num_rows*num_cols
        
        # it's really one long sorted array
        class ChoppedArray:
            def __len__(self):
                return num_elems
            def __getitem__(self, idx):
                if num_rows == 1 or idx < num_cols:
                    return matrix[0][idx]
                elif num_cols == 1:
                    return matrix[idx][0]
                row_idx = idx // num_cols
                col_idx = idx - row_idx*num_cols
                return matrix[row_idx][col_idx]
        
        from bisect import bisect_left
        ca = ChoppedArray()
        i = bisect_left(ca, target)
        return i != len(ca) and ca[i] == target
    