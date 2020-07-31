def containsDuplicate(nums: List[int]) -> bool:
    seen = set()
    for i in nums:
        if i in seen:
            return True
        else:
            seen.add(i)
    return False