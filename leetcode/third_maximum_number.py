class Solution:
    # short of soring the array, iterating through it 3 times
    # would probably be a cleaner solution?
    def thirdMax(self, nums: List[int]) -> int:
        
        # let's try this in one pass!
        first = second = third = None
        
        for i in nums:
            if first is None:
                first = i
            elif second is None:
                if i > first:
                    first, second = i, first
                else:
                    second = i if i != first else None
            elif third is None:
                if i > first:
                    third, second, first = second, first, i
                elif i > second:
                    if i != first:
                        third, second = second, i
                else:
                    if i != second:
                        third = i
            elif i > first:
                third, second, first = second, first, i
            elif i > second:
                if i != first:
                    second, third = i, second
            elif i > third:
                if i != second:
                    third = i
        
        # return the max is there's no third maximum
        return third if third is not None else first