def firstUniqChar(s: str) -> int:
        seen = set()
        dupes = set()
        for idx, c in enumerate(s):
            if c in seen:
                dupes.add(c)
            else:
                seen.add(c)
        for idx, c in enumerate(s):
            if c not in dupes:
                return idx
        return -1