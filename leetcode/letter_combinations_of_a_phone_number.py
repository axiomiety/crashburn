class Solution:
    def letterCombinations(self, digits: str) -> List[str]:
        letters = {
            '2': ('a','b','c'),
            '3': ('d','e','f'),
            '4': ('g','h','i'),
            '5': ('j','k','l'),
            '6': ('m','n','o'),
            '7': ('p','q','r','s'),
            '8': ('t','u','v'),
            '9': ('w','x','y','z')
        }
        if not digits:
            return []
        stems = ['']
        for digit in digits:
            new_stems = []
            for letter in letters[digit]:
                for stem in stems:
                    new_stems.append(f"{stem}{letter}") # though using stem+letter seems faster
            stems = new_stems
        return stems
        