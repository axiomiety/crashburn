def isPowerOfThree(n: int) -> bool:
    # special case - 3^0
    if n == 1:
        return True
    while n > 0 and (n % 3) == 0:
        n = n/3
        if n == 1:
            return True
    return False