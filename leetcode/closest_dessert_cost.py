class Solution:
    def closestCost(self, baseCosts: List[int], toppingCosts: List[int], target: int) -> int:
        
        def get_closest_toppings(new_target):
            running_sums = set()
            # we can have at most 2 of each topping
            all_possible_toppings = deque(toppingCosts + toppingCosts)
            while all_possible_toppings:
                topping = all_possible_toppings.popleft()
                new_sums = set()
                for running_sum in running_sums:
                    if running_sum < new_target:
                        new_sums.add(topping + running_sum)
                running_sums.add(topping)
                running_sums.update(new_sums)
            return get_closest_diff(new_target, running_sums)
            
        def get_closest_diff(new_target, running_sums):
            closest_sum, closest_diff = None, None
            closest_diff = new_target
            for running_sum in running_sums:
                diff = abs(new_target-running_sum)
                if not closest_sum or diff < closest_diff:
                    closest_sum, closest_diff = running_sum, diff
                elif diff == closest_diff:
                    closest_sum = min(closest_sum, running_sum)
            return closest_sum
        
        from collections import deque
        costs = set()
        for base in baseCosts:
            costs.add(base) # base only, no toppings
            if base < target:
                costs.add(base+get_closest_toppings(target-base))
        
        return get_closest_diff(target, costs)