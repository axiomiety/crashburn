from typing import List
def moveZeroes(nums: List[int]) -> None:
    offset = 0
    num_zeroes = 0
    idx = 0
    length = len(nums)
    while idx < length:
        val = nums[idx]
        if val == 0:
            num_zeroes += 1
        else:
            nums[offset] = val
            offset +=1
        idx += 1
    for i in range(length-num_zeroes, length):
        nums[i] = 0