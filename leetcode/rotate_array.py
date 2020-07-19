from typing import List

def rotate(nums: List[int], k: int) -> None:
    displaced = {}
    n = len(nums)
    for i in range(n):
        new_pos = (i+k)%n
        if i in displaced:
            val = displaced[i]
            del displaced[i]
        else:
            val = nums[i]
        displaced_val = nums[new_pos]
        nums[new_pos] = val
        displaced[new_pos] = displaced_val
        

if __name__ == "__main__":
    nums = [-1,-100,3, 99]
    rotate(nums, 2)
    assert nums == [3,99,-1,-100], "woops"