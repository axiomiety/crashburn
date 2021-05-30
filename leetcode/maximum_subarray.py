class Solution:
    def maxSubArray(self, nums: List[int]) -> int:
        max_ending = nums[0]
        best = max_ending
        for val in nums[1:]:
            max_ending = max(max_ending + val, val)
            if max_ending > best:
                best = max_ending
        return best