class Solution:
    def checkIfExist(self, arr: List[int]) -> bool:
        doubles = set()
        for elem in arr:
            if elem*2 in doubles or (elem % 2 == 0 and elem//2 in doubles):
                return True
            else:
                doubles.add(elem)
        return False