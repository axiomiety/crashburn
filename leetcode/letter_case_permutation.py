class Solution:
    def letterCasePermutation(self, s: str) -> List[str]:
        from collections import deque
        def backtrack(elements, stems):
            if not elements:
                return stems
            
            item = elements.popleft()
            new_stems = []
            for stem in stems:
                if item.isalpha():
                    new_stems.append(stem + item.lower())
                    new_stems.append(stem + item.upper())
                else:
                    new_stems.append(stem+item)
            return backtrack(elements, new_stems)
        
        return backtrack(deque(s),[""])