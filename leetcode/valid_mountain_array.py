class Solution:
    def validMountainArray(self, arr: List[int]) -> bool:
        if len(arr) < 3:
            return False
        
        peak_idx = None
        idx = 1
        prev, curr = arr[0], arr[idx]
        
        # find the peak first
        while curr > prev and idx < len(arr)-1:
            peak_idx = idx
            prev = curr
            idx += 1
            curr = arr[idx]
        
        if peak_idx is None: # didn't go up at all!
            return False
        
        return all(arr[idx] > arr[idx+1] for idx in range(peak_idx, len(arr)-1))