def strStr(haystack: str, needle: str) -> int:
    base = 0
    idx = 0
    n = len(haystack)
    m = len(needle)
    while base + idx < n:
        if idx == m:
            return base
        else:
            if needle[idx] == haystack[base+idx]:
                idx += 1
            else:
                j = idx
                while j > 0 and needle[j] != haystack[base+idx]:
                    j -= 1
                base += max(idx-j,1)
                idx = 0
    return base if idx == m else -1