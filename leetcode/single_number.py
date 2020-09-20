def singleNumber_pythonic(nums: List[int]) -> int:
    from collections import Counter
    counter = Counter(nums)
    for num, count in counter.items():
        if count == 1:
            return num