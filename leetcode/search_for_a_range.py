class Solution:
    def searchRange(self, nums: List[int], target: int) -> List[int]:
        if not nums:
            return [-1,-1]
        if len(nums) == 1:
            return [0,0] if nums[0] == target else [-1,-1]
        
        def bin_search():
            start, end = 0, len(nums)-1
            while start <= end:
                mid = start + (end-start)//2
                if nums[mid] == target:
                    return mid
                elif nums[mid] < target:
                    start = mid+1
                else:
                    end = mid-1
            return -1
        
        # find the target anywhere in nums
        pivot = bin_search()
       
        if pivot == -1:
            return [-1, -1]

        # now find the starting point
        start, end = 0, pivot
        while start <= end:
            if nums[start] == nums[end]:
                break
            else:
                mid = start + (end-start)//2
                if nums[mid] < nums[end]:
                    start = mid+1
                else:
                    end = mid
        range_start = start
        
        # and now find the end
        start, end = pivot, len(nums)-1
        while start <= end:
            if nums[end] == target:
                return [range_start, end]
            else:
                mid = start + (end-start)//2
                if target < nums[mid]:
                    end = mid-1
                else:
                    start = mid+1
        # as we always round down, we may be +1
        range_end = start if nums[start] == target else start-1
        return [range_start, range_end]
        