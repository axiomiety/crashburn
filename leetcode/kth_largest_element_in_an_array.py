class Solution:
    def findKthLargest(self, nums: List[int], k: int) -> int:
        import heapq
        smallest = -10**4
        kth_largest = [smallest for _ in range(k)]
        for num in nums:
            heapq.heappushpop(kth_largest, num)
        return heapq.heappop(kth_largest)