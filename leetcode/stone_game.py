class Solution:
    def stoneGame(self, piles: List[int]) -> bool:
        import functools
        
        @functools.cache
        def best_scores(piles):
            if len(piles) == 2:
                # Alice always goes first
                return max(piles),min(piles)
            else:
                best_left = best_scores(piles[1:])
                best_right = best_scores(piles[:-1])
                
                if len(piles) % 2 == 0:
                    # Alice's turn
                    max_left = piles[0] + best_left[0]
                    max_right = piles[-1] + best_right[0]
                    if max_left > max_right:
                        return max_left,best_left[1]
                    else:
                        return max_right,best_right[1]
                else:
                    # Bob's turn
                    max_left = piles[0] + best_left[1]
                    max_right = piles[-1] + best_right[1]
                    if max_left > max_right:
                        return best_left[0],max_left
                    else:
                        return best_right[0],max_right
        best_alice, best_bob = best_scores(tuple(piles))
        return best_alice > best_bob