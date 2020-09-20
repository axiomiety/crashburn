from typing import List
def maxProfit(prices: List[int]) -> int:
    # arg - got bitten by 0 in a boolean context
    holding = False
    transactions = []
    buyAt = None
    sellAt = None
    for idx in range(len(prices)-1,-1,-1):
        today = prices[idx]
        if not holding:
            sellAt = today
            holding = True
        else:
            if buyAt is not None and today > buyAt:
                transactions.append((buyAt, sellAt))
                buyAt = None
                sellAt = today
            elif buyAt is None and today > sellAt:                
                sellAt = today
            elif buyAt is None or today < buyAt:
                buyAt = today
    if buyAt is not None and sellAt is not None:
        transactions.append((buyAt, sellAt))
    profit = 0
    for b,s in transactions:
        profit += s-b
    return profit