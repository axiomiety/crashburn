class Solution:
    def heightChecker(self, heights: List[int]) -> int:
        sorted_heights = sorted(heights)
        diff_count = 0
        for a, b in zip(heights, sorted_heights):
            if a != b:
                diff_count += 1
        return diff_count