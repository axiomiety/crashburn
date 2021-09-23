class Solution:
    cache = {}
    @staticmethod
    def compress(digits: str) -> str:
        if digits not in Solution.cache:
            ret = []
            curr, count = digits[0], 1
            for val in digits[1:]:
                if val == curr:
                    count += 1
                else:
                    ret.append(count)
                    ret.append(curr)
                    curr = val
                    count = 1
            ret.append(count)
            ret.append(curr)
            Solution.cache[digits] = ''.join(str(i) for i in ret)
        return Solution.cache[digits]
    
    def countAndSay(self, n: int) -> str:
        if n == 1:
            return "1"
        acc = "1"
        for _ in range(1, n):
            acc = Solution.compress(acc)
        return acc