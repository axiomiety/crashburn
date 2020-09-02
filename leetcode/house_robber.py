def rob(nums: List[int]) -> int:
        n = len(nums)
        if n == 0:
            return 0
        elif n == 1:
            return nums[0]
        
        mem = [nums[0]]
        mem.append(max(nums[0],nums[1]))
        for idx in range(2, n):
            mem.append(max(nums[idx] + mem[idx-2], mem[idx-1]))
        return mem[-1]

# slow!
def rob_recursive(nums: List[int]) -> int:
    def _rob(arr):
        n = len(arr)
        if n == 0:
            return 0
        elif n == 1:
            return arr[0]
        elif n == 2:
            return max(arr)
        elif n == 3:
            return max(arr[0] + arr[2], arr[1])
        else:
            return max(arr[0] + _rob(arr[2:]), arr[1] + _rob(arr[3:]))
    return _rob(nums)

def rob_naive_memoize(self, nums: List[int]) -> int:
    mem = {}
    def _rob(arr):
        n = len(arr)
        if n in mem:
            return mem[n]
        if n == 0:
            mem[n] = 0
        elif n == 1:
            mem[n] = arr[0]
        elif n == 2:
            mem[n] = max(arr)
        elif n == 3:
            mem[n] = max(arr[0] + arr[2], arr[1])
        else:
            mem[n] = max(arr[0] + _rob(arr[2:]), arr[1] + _rob(arr[3:]))
        return mem[n]
    return _rob(nums)