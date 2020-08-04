import string
def isPalindrome(s: str) -> bool:
    normalised_s = [c for c in s.lower() if c in string.ascii_lowercase or c in string.digits]
    # technically we only need to check len(normalised_s)/2 items
    return all(a == b for a,b in zip(normalised_s, reversed(normalised_s)))