class Solution:
    def findNumbers(self, nums: List[int]) -> int:
        def is_even_number_digits(num):
            return  10 <= num <= 99 or 1000 <= num <= 9999 or 100000 <= num <= 999999
        return len([num for num in nums if is_even_number_digits(num)])