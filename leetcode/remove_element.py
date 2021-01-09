class Solution:
    def removeElement(self, nums: List[int], val: int) -> int:
        copy_from, copy_to, count = 0, 0, 0
        while copy_from < len(nums):
            if nums[copy_from] == val:
                copy_from += 1                
            else:
                if copy_from > copy_to:
                    nums[copy_to] = nums[copy_from]
                copy_from += 1
                copy_to += 1                
                count += 1            
        return count