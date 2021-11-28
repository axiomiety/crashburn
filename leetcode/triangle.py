class Solution:
    def minimumTotal(self, triangle: List[List[int]]) -> int:
        height = len(triangle)
        def reduce_row(row):
            # assume len(row) > 1
            assert len(row) > 1
            reduced_row = []
            for i, j in zip(row, row[1:]):
                reduced_row.append(min(i,j))
            return reduced_row
        
        for h in range(height-1,0, -1):
            reduced_row = reduce_row(triangle[h])
            for idx, r in enumerate(reduced_row):
                triangle[h-1][idx] += r
        return triangle[0][0]