from collections import Counter
def isAnagram(s: str, t: str) -> bool:
    return Counter(s) == Counter(t)