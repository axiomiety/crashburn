def firstBadVersion(n: int):
    """
    :type n: int
    :rtype: int
    """
    
    start, end = 1, n
    while True:
        midpoint = start + (end-start) // 2
        if isBadVersion(midpoint):
            # then it must be before
            end = midpoint
        else:
            # then it's after
            start = midpoint
        
        if end - start <= 1:
            return start if isBadVersion(start) else end