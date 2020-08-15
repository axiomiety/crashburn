def climbStairs1(n: int) -> int:
    ways = {}
    ways[1] = 1
    ways[2] = 2
    k = 2
    ret = 2
    while k < n:
        k += 1
        ways[k] = ways[k-1] + ways[k-2]
    return ways[n]

def climbStairs2(n: int) -> int:
    step_minus2 = 1
    step_minus1 = 2
    # base cases
    if n == 1:
        return 1
    if n == 2:
        return 2
    k = 2
    ret = None
    # loop
    while k < n:
        ret = step_minus1 + step_minus2
        step_minus2 = step_minus1
        step_minus1 = ret
        k += 1
    return ret