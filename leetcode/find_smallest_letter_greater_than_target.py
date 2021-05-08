class Solution:
    def nextGreatestLetter(self, letters: List[str], target: str) -> str:
        if target < letters[0] or target > letters[-1]:
            return letters[0]
        
        def find_idx():
            count = 0
            start, stop = 0, len(letters)-1
            while start < stop:
                mid = start + (stop-start)//2
                pivot = letters[mid]
                if pivot == target:
                    return mid
                elif target > pivot:
                    start = mid+1
                else:
                    stop = mid-1
            return start
        
        idx = find_idx()
        if letters[idx] == target:
            while letters[idx%len(letters)] == target:
                idx += 1
            return letters[idx%len(letters)]
        return letters[(idx+1)%len(letters)] if letters[idx] <= target else letters[idx%len(letters)]
        