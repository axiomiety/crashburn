def isArrValid(arr: List[str], out=False) -> bool:
    vals = set()
    count = 0
    for val in arr:
        if out:
            print(val)
        if val == '.':
            next
        else:
            # only add digits
            vals.add(val)
            count += 1
    return len(vals) == count

# iterators rock
def transpose(arr: List[List[str]], col: int) -> str:
    for row in range(9):
        yield arr[row][col] 

# and rock some more
def box(arr: List[List[str]], r:int, c: int) -> str:
    for row_offset in range(3):
        for col_offset in range(3):
            yield arr[r*3+row_offset][c*3+col_offset]

def isValidSudoku(board: List[List[str]]) -> bool:
    # every row must be valid
    areRowsValid = all(isArrValid(row) for row in board)
    if not areRowsValid:
        return False
    # every column must be valid
    areColumnsValid = all(isArrValid(transpose(board, col)) for col in range(9))
    if not areColumnsValid:
        return False
    # every 3x3 box must be valid
    for r in range(3):
        for c in range(3):
            if not isArrValid(box(board, r,c)):
                return False
    return True