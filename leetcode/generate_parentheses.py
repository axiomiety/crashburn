class Solution:
    def generateParenthesis(self, n: int) -> List[str]:
        def gen(k):
            if k == 1:
                yield '(',1
                yield ')',-1
            else:
                for pattern, parity in gen(k-1):
                    if parity >= 1:
                        yield f'{pattern})',parity-1
                    if parity >= 0:
                        yield f'{pattern}(',parity+1
                    # no point if parity would be negative
       
        return set(pattern for pattern, parity in gen(n*2) if parity == 0)