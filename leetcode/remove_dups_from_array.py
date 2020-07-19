def removeDuplicates(nums: List[int]) -> int:
    count = 0
    curr = None
    for elem in nums:
        if elem == curr:
            next
        else:
            curr = elem
            nums[count] = curr
            count += 1
            
    return count