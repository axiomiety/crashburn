class Solution:
    def search(self, nums: List[int], target: int) -> int:
        start, end = 0, len(nums) - 1
    
        if len(nums) == 1:
            return 0 if nums[0] == target else -1
    
        # [5,6,1,2,3,4]
        if nums[start] > nums[end]:
            # we can do better!
            pivot = nums.index(min(nums))
        else:
            pivot = len(nums)-1
        
        start, end = 0, len(nums) - 1
       
        if nums[start] <= target <= nums[pivot-1]:
            end = pivot-1
        else:
            start = pivot
     
        while start <= end:
            mid = start + (end-start)//2
            if nums[mid] == target:
                return mid
            elif nums[mid] > target:
                end = mid-1
            else:
                start = mid+1
                
        return -1