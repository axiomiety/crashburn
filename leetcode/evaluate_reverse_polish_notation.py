class Solution:
    def evalRPN(self, tokens: List[str]) -> int:
        op_stack = []
        for token in tokens:
            if token in ("+","-","*","/"):
                op_right = op_stack.pop()
                op_left = op_stack.pop()
                if token == "+":
                    ret = op_left + op_right
                elif token == "-":
                    ret = op_left - op_right
                elif token == "*":
                    ret = op_left * op_right
                else:
                    ret = trunc(op_left / op_right)
                op_stack.append(ret)
            else:
                op_stack.append(int(token))
        return op_stack.pop()