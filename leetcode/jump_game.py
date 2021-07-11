class Solution:
     def canJump(self, nums: List[int]) -> bool:
        if len(nums) == 1:
            return True
        max_idx = nums[0]
        last_idx = len(nums)-1
        for idx, val in enumerate(nums):
            # we can jump straight to the end
            if max_idx >= last_idx:
                return True
            # we can't reach our current position!
            elif idx > max_idx:
                return False
            # end case
            if idx == max_idx and val == 0 and idx != last_idx:
                return False
            # otherwise, see how far we can jump from here
            max_idx = max(max_idx, idx+val)
        return False