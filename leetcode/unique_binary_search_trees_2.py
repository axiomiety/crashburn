# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    # okay that's ugly, and shortcuts were taken. but it passed!
    def generateTrees(self, n: int) -> List[TreeNode]:
        # tree must not be None!
        def ins(tree, val):
            if val < tree.val:
                if tree.left:
                    ins(tree.left, val)
                else:
                    tree.left = TreeNode(val=val)
            else:
                if tree.right:
                    ins(tree.right, val)
                else:
                    tree.right = TreeNode(val=val)
        def parse(tree):
            path = []
            nodes = [tree]
            while nodes:
                node = nodes.pop()
                if node:
                    path.append(node.val)
                    nodes.append(node.left)
                    nodes.append(node.right)
            return tuple(path)
        
        values = {v for v in range(1,n+1)}
        # this essentially creates all the permutations
        # could have used a built-in most likely
        pipeline = [[v] for v in values]
        insert_paths = []
        while pipeline:
            path = pipeline.pop()
            if len(path) == n:
                insert_paths.append(path)
            # remaining digits
            for value in values.difference(set(path)):
                pipeline.append([*path,value])
        
        ret = []
        seen = set()
        # now create trees for each insert path
        for path in insert_paths:
            root = TreeNode(val=path.pop())
            for val in path:
                ins(root, val)
            computed_path = parse(root)
            if computed_path in seen:
                continue
            seen.add(computed_path)
            ret.append(root)
            
        return ret