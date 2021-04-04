class Solution:
    def findPeakElement_naive(self, nums: List[int]) -> int:
        if not nums:
            return -1
        if len(nums) == 1:
            return 0
        
        start, end = 0, len(nums)
        
        def is_peak(idx):
            if idx == 0:
                return nums[idx] > nums[idx+1]
            elif idx == len(nums)-1:
                return nums[idx] > nums[idx-1]
            else:
                return nums[idx-1] < nums[idx] and nums[idx] > nums[idx+1]
            
        for i in range(start, end):
            if is_peak(i):
                return i
        return -1