def lengthOfLongestSubstring1(s: str) -> int:
    n = len(s)
    m, idx = 0, 0
    substr = []
    while idx < n:
        letter = s[idx]
        #print(f"{substr},{letter},idx:{idx},m:{m}")
        if letter not in substr:
            substr.append(s[idx])
            if len(substr) > m:
                m = len(substr)
            idx += 1
        else:
            idx -= len(substr) - substr.index(letter) - 1
            #print(f"resetting to {idx}")
            substr = []
    return m

def lengthOfLongestSubstring(s: str) -> int:
    n = len(s)
    curr_max = idx = 0
    substr_start = idx
    offset = substr_start + idx
    while offset <= n:
        if s[offset] not in s[substr_start:offset]:
            curr_max = max(curr_max, offset-substr_start+1)
            idx += 1
        else:
            substr_start += s[substr_start:offset].index(s[offset]) + 1 
            idx = 0
        offset = substr_start + idx
    return curr_max