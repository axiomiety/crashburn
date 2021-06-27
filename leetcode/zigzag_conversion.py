class Solution:
    def convert(self, s: str, numRows: int) -> str:
        if numRows == 1:
            return s
        
        letters = list(s)
        cycle_length = numRows + numRows-2
        def get_offset(row_idx):
            while True:
                if row_idx == 0 or row_idx == numRows - 1:
                    yield cycle_length
                else:
                    yield cycle_length-2*row_idx
                    yield 2*row_idx
        
        def get_letter():
            for row_idx in range(numRows):
                idx = row_idx
                gen = get_offset(row_idx)
                while idx < len(s):
                    yield s[idx]
                    offset = next(gen)
                    idx += offset
        return ''.join(letter for letter in get_letter())

