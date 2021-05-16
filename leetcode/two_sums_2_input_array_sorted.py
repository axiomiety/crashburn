class Solution:
    def twoSum(self, numbers: List[int], target: int) -> List[int]:
        import bisect
        idx = 0
        while idx < len(numbers)-1:
            complement = target - numbers[idx]
            complement_idx = bisect.bisect_left(numbers, complement, idx+1)
            if complement_idx != len(numbers) and numbers[complement_idx] == complement: # the complement is in the interval!
                return [idx+1, complement_idx+1]
            idx += 1
        return [None, None]