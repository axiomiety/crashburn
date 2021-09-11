class Solution:
    def simplifyPath(self, path: str) -> str:
        stack = []
        for token in path.split('/'):
            if token == '':
                continue
            elif token == '..':
                if stack:
                    stack.pop()
            elif token == '.':
                continue
            else:
                stack.append(token)
        return '/'+'/'.join(stack)