class Solution:
    def replaceElements(self, arr: List[int]) -> List[int]:
        if len(arr) > 1:
            new_max = arr[-1]
            for idx in range(len(arr)-2, -1, -1):
                tmp = arr[idx]
                arr[idx] = max(arr[idx+1], new_max)
                new_max = max(tmp, new_max)
                
        arr[-1] = -1
        return arr