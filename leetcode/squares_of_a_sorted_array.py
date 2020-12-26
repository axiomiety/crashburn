class Solution:
    def sortedSquares_iterate_once(self, nums: List[int]) -> List[int]:
        start, end = 0, len(nums)-1
        ret = []
        while (end-start) >= 0:
            sq_left = nums[start]**2
            sq_right = nums[end]**2
            if sq_right > sq_left:
                ret.append(sq_right)
                end -= 1
            else:
                ret.append(sq_left)
                start += 1
                
        return reversed(ret)
            
    
    def sortedSquares_simple(self, nums: List[int]) -> List[int]:
        return sorted(x**2 for x in nums)