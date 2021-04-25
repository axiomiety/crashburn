class Solution:
    def findClosestElements(self, arr: List[int], k: int, x: int) -> List[int]:
        def is_closer(a,b):
            return abs(a-x) < abs(b-x) or (abs(a-x) == abs(b-x) and a < b)
        
        def find_x():
            start, end = 0, len(arr)-1
            while start <= end:
                mid = start + (end-start)//2
                if arr[mid] == x:
                    return mid
                elif arr[mid] < x:
                    start = mid+1
                else:
                    end = mid-1
            # start > end
            return end if is_closer(arr[end], arr[start]) else start
        
        # x is outside the boundaries of the array
        if x <= arr[0]:
            return arr[:k]
        elif x >= arr[-1]:
            return arr[-k:]

        # x may not be in arr
        pivot = find_x()
        count, ret = 1, [arr[pivot]]
        left, right = pivot-1, pivot+1
        while count < k:
            #print(count, ret, left, right)
            # check if we reached either end of the array
            if left < 0:
                ret.append(arr[right])
                right += 1
            elif right >= len(arr):
                ret.insert(0,arr[left])
                left -= 1
            elif is_closer(arr[left], arr[right]):
                ret.insert(0, arr[left])
                left -= 1
            else:
                ret.append(arr[right])
                right += 1
            count += 1
        return ret