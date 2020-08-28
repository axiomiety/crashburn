from typing import List
# brute-force
def twoSum(nums: List[int], target: int) -> List[int]:
    for i in range(0, len(nums)-1):
        for j in range(i+1, len(nums)):
            if nums[i] + nums[j] == target:
                return [i, j]

# this is somehow faster than the hash-based approach
def twoSum2(nums: List[int], target: int) -> List[int]:
    # n log n - we need to keep the original list as we care about the indexes
    nums_sorted = sorted(nums)
    start, end = 0, len(nums)-1
    found = False
    while not found:
        s = nums_sorted[start] + nums_sorted[end]
        if s == target:
            found = True
        elif s > target:
            # reset start, decrement end
            start, end = 0, end-1
        else:
            start += 1
    actual_start = nums.index(nums_sorted[start])
    # this is to deal with dupes
    if nums_sorted[start] == nums_sorted[end]:
        actual_end = nums.index(nums_sorted[end], actual_start+1)
    else:
        actual_end = nums.index(nums_sorted[end])
    return [actual_start, actual_end]

def twoSum3(nums: List[int], target: int) -> List[int]:
    complements = {}
    for idx, n in enumerate(nums):
        diff = target - n
        if diff in complements:
            return [idx, complements[diff]]
        else:
            complements[n] = idx