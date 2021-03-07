class Solution:
    def search(self, nums: List[int], target: int) -> int:
        start, end = 0, len(nums)-1
        while start <= end:
            mid = start + (end-start) // 2
            val = nums[mid]
            if val > target:
                end = mid-1 if end == mid else mid
            elif val < target:
                start = mid+1 if start == mid else mid
            else:
                return mid
        return -1