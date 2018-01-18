import copy
import math

def col(board, idx):
    return [row[idx] for row in board]

def row(board, idx):
    return board[idx]

def isValid(cons):
    ''' a list of digits is valid iff there are no duplicates
        and 1 <= k <= n for all k, where n is the width of the board '''
    n = len(cons)
    # we filter out potential None
    digits = [c for c in cons if c is not None]
    return len(digits) == len(set(digits)) and all(1 <= k <= n for k in digits)

def boardChunks(board):
    chunks = []
    n = len(board)
    chunkSize = int(math.sqrt(n))
    for r in range(chunkSize):
        rows = board[r*chunkSize:(r+1)*chunkSize]
        for c in range(chunkSize):
            chunk = []
            for rr in rows:
                chunk.extend(rr[c*chunkSize:(c+1)*chunkSize])
            chunks.append(chunk)
    
    return chunks

def isBoardValid(board):
    n = len(board)
    return all(isValid(col(board, idx)) and isValid(row(board,idx)) for idx in range(n)) \
            and \
            all(isValid(chunk) for chunk in boardChunks(board))

def isBoardComplete(board):
    return not any(map(lambda rr: None in rr, board))

def getFullestRowIdx(board):
    minNones = len(board) # worst case scenario, row is totally empty
    fullestRow = 0
    for rowIdx in range(len(board)):
        r = row(board, rowIdx)
        c = r.count(None)
        if 0 < c < minNones:
            fullestRow = rowIdx
            minNones = c
    
    return fullestRow

def genValidRows(board, rowIdx):
    r = row(board, rowIdx)
    rows = [r]
    valid_rows = []
    while rows:
        r = rows.pop()
        emptyIdx = r.index(None)
        for val in range(1, len(r)+1):
            new_row = r[:]
            new_row[emptyIdx] = val
            if isValid(new_row) and isBoardValid(merge(board, new_row, rowIdx)):
                if None in new_row:
                    rows.append(new_row)
                else:
                    valid_rows.append(new_row)

    return valid_rows

def merge(board, row, rowIdx):
    new_board = copy.deepcopy(board)
    new_board[rowIdx] = row
    return new_board

_sampleBoard = [
    [5,3,None,None,7,None,None,None,None],
    [6,None,None,1,9,5,None,None,None,],
    [None,9,8,None,None,None,None,6,None],
    [8,None,None,None,6,None,None,None,3],
    [4,None,None,8,None,3,None,None,1],
    [7,None,None,None,2,None,None,None,6],
    [None,6,None,None,None,None,2,8,None],
    [None,None,None,4,1,9,None,None,5],
    [None,None,None,None,8,None,None,7,9]
]

def solve(board):
    boards = [board]

    while boards:
        b = boards.pop()

        if isBoardComplete(b) and isBoardValid(b): # technically the board should already by valid
            return b # a potential solution

        candidate_row_idx = getFullestRowIdx(b)

        for possible_row in genValidRows(b, candidate_row_idx):
            boards.append(merge(b, possible_row, candidate_row_idx))
        
    return [] # no solution

import unittest

class SudokuTest(unittest.TestCase):

    def test_boardAccessors(self):
        b = [[1,2],[3,4]]
        self.assertEqual(row(b,0), [1,2])
        self.assertEqual(row(b,1), [3,4])
        self.assertEqual(col(b,0), [1,3])
        self.assertEqual(col(b,1), [2,4])

        # this is not a valid board, we just want to make sure we get unique chunks
        b = [
                [1, 2, 3, 4],
                [5, 6, 7, 8],
                [-1, -2, -3, -4],
                [-5, -6, -7, -8]
            ]
        chunks = boardChunks(b)
        self.assertEqual(chunks[0], [1,2,5,6])
        self.assertEqual(chunks[1], [3,4,7,8])
        self.assertEqual(chunks[2], [-1,-2,-5,-6])
        self.assertEqual(chunks[3], [-3,-4,-7,-8])

    def test_validity(self):
        b = [
                [1, 2, 3, 4],
                [4, 3, 2, 1],
                [3, 4, 1, 2],
                [2, 1, 4, 3]
            ]
        self.assertTrue(isBoardComplete(b))
        self.assertTrue(isBoardValid(b))
        b[0][0] = None
        self.assertFalse(isBoardComplete(b))
        self.assertTrue(isBoardValid(b))
        # rows and columns are valid but chunks aren't
        b = [
                [1, 2, 3, 4],
                [2, 3, 4, 1],
                [3, 4, 1, 2],
                [4, 1, 2, 3]

            ]
        self.assertFalse(isBoardValid(b))

    def test_getFullestRowIdx(self):
        # the fullest row is the first one, but it has no empty cells
        # so we're not interested
        b = [
                [1,2,3],
                [None, None, None],
                [None, 1, None],
                [None, 1, 2]
            ]
        self.assertEqual(getFullestRowIdx(b), 3)

    def test_genValidRows(self):
        b = [
                [1, None, None, None],
                [None, None, None, 1],
                [None, None, None, None],
                [2, 1, 4, 3]
            ]
        valid_rows = [[4, 3, 1, 2], [3, 4, 1, 2]]
        self.assertEqual(genValidRows(b, 2), valid_rows)
        valid_rows = [[1, 4, 3, 2], [1, 3, 2, 4], [1, 2, 3, 4]]
        self.assertEqual(genValidRows(b, 0), valid_rows)

if __name__ == '__main__':
    unittest.main()