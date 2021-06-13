class Solution:
    def isValid(self, s: str) -> bool:
        close_map = {')': '(','}':'{',']':'['}
        
        stack = []
        for val in s:
            if val in close_map:
                top_of_stack = stack.pop() if stack else None # case for when stack is empty
                if close_map[val] == top_of_stack:
                    continue
                else:
                    return False
            else:
                stack.append(val)
        return len(stack) == 0