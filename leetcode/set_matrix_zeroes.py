def setZeroes(matrix: List[List[int]]) -> None:
    """
    Do not return anything, modify matrix in-place instead.
    """
    zcols = set()
    for ridx in range(len(matrix)):
        for cidx in range(len(matrix[ridx])):
            if matrix[ridx][cidx] == 0:
                # zero out the row
                for k in range(len(matrix[ridx])):
                    if matrix[ridx][k] == 0:
                        zcols.add(k)
                    matrix[ridx][k] = 0
                break
    for cidx in zcols:
        for ridx in range(len(matrix)):
            matrix[ridx][cidx] = 0