from typing import List

def rotate_naive(matrix: List[List[int]]) -> None:
    """
    Do not return anything, modify matrix in-place instead.
    """
    n = len(matrix)
    rot = []
    for col in range(n):
        new_row = []
        for row in range(n):
            new_row.append(matrix[n-1-row][col])
        rot.append(new_row)
    for r in range(n):
        for c in range(n):
            matrix[r][c] = rot[r][c]

def rotate_least_space(matrix: List[List[int]]) -> None:
    """
    Much slower than the naive solution it seems!
    """
    n = len(matrix)
    def rot(r,c):
        return (c, n-1-r)
    for r in range(n-1):
        for c in range(r,n-1-r):
            rr, rc = r, c
            temp = matrix[rr][rc]
            # 4 rotations
            for i in range(4):
                new_rr, new_rc = rot(rr, rc)               
                new_temp = matrix[new_rr][new_rc]
                matrix[new_rr][new_rc] = temp
                temp = new_temp
                rr, rc = new_rr, new_rc
