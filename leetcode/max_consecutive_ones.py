class Solution:
    def findMaxConsecutiveOnes(self, nums: List[int]) -> int:
        max_count = 0
        running_count = 0
        for i in nums:
            if i == 0:
                max_count = max(max_count, running_count)
                running_count = 0
            else:
                running_count += 1
        return max(max_count,running_count)