class Solution:
    def generate(self, numRows: int) -> List[List[int]]:
        if numRows == 1:
            return [[1]]
        elif numRows == 2:
            return [[1],[1,1]]
        rows = [[1],[1,1]]
        for rowIdx in range(3, numRows+1):
            prevRow = rows[-1]
            newRow = [1]
            for idx in range(rowIdx-2):
                newRow.append(prevRow[idx]+prevRow[idx+1])
            newRow.append(prevRow[-1])
            rows.append(newRow)
        return rows