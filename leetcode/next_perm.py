class Solution:
    def nextPermutation(self, nums: List[int]) -> None:
        """
        Do not return anything, modify nums in-place instead.
        """
        if len(nums) == 1:
            return
        
        
        def find_smallest_min(start):
            next_smallest, next_smallest_idx = None, None
            pivot = nums[start]
            for idx, val in enumerate(nums[start:]):
                if val > pivot:
                    if next_smallest and val < next_smallest:
                        next_smallest = val
                        next_smallest_idx = idx + start
                    else:
                        next_smallest = val
                        next_smallest_idx = idx + start
            return (next_smallest, next_smallest_idx)
        
        for i in range(len(nums)):
            start = len(nums)-i-1
            next_smallest, next_smallest_idx = find_smallest_min(start)
            if next_smallest:
                nums[start], nums[next_smallest_idx] = nums[next_smallest_idx], nums[start]
                # the tail is in reverse order
                if start < len(nums)-2:
                    nums[start+1:] = nums[len(nums)-1:start:-1]
                return
        # if we're still here, we didn't find anything
        nums.sort()