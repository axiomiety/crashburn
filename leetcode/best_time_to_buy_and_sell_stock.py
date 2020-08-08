class Solution:
    # naive1
    def naive1(self, prices: List[int]) -> int:
        max_profit = 0
        for idx, buy in enumerate(prices):
            for sell in prices[idx:]:
                if sell > buy and sell - buy > max_profit:
                    max_profit = sell-buy
        return max_profit
    
    def maxProfit(self, prices: List[int]) -> int:
        if not prices:
            return 0
        # find the max in each subinterval
        max_in_subinterval = []
        m = prices[-1]
        for price in reversed(prices):
            if price > m:
                max_in_subinterval.append(price)
                m = price
            else:
                max_in_subinterval.append(m)
        # given we went at it backwards
        max_in_subinterval = list(reversed(max_in_subinterval))
        max_profit = 0
        for idx, buy in enumerate(prices[:-1]):
            sell = max_in_subinterval[idx+1]
            if sell > buy and sell-buy > max_profit:
                max_profit = sell-buy
        return max_profit