class Solution:
    def findRadius(self, houses: List[int], heaters: List[int]) -> int:
        houses_ = sorted(houses)
        heaters_ = sorted(heaters)
        heater_idx = 0
        max_min_radius = 0
        def get_heaters(house):
            nonlocal heater_idx
            if heater_idx >= len(heaters_):
                return heaters_[heater_idx-1], None
            elif heaters_[heater_idx] == house:
                return heaters_[heater_idx], heaters_[heater_idx]
            elif heaters_[heater_idx] > house:
                low = heaters_[heater_idx-1] if heater_idx > 0 else None
                high = heaters_[heater_idx]
                return low, high
            else:
                heater_idx += 1
                return get_heaters(house)
            
        for house in houses_:
            low, high = get_heaters(house)
            min_dist = min(house-low if low else high-house, high-house if high else house-low)
            max_min_radius = max(max_min_radius, min_dist)
        return max_min_radius