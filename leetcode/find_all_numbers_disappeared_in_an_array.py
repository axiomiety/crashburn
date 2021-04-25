class Solution:
    def findDisappearedNumbers(self, nums: List[int]) -> List[int]:
        bit_array = 0
        for num in nums:
            bit_array |= 2**num
        ret = []
        for i in range(1, len(nums)+1):
            if bit_array >> i & 1 == 0:
                ret.append(i)
                
        return ret