class Solution:
    def maxArea(self, height: List[int]) -> int:
        left, right = 0, len(height)-1
        max_container_area = (right-left)*min(height[left],height[right])
        while left < right:
            container_area = (right-left)*min(height[left],height[right])
            max_container_area = max(container_area, max_container_area)
            if height[left] > height[right]:
                right -= 1
            else:
                left += 1
        return max_container_area