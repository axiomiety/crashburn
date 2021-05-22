class Solution:
    def isSymmetric(self, root: TreeNode) -> bool:
        def is_same(left, right):
            if not left and not right:
                return True
            elif left and not right:
                return False
            elif not left and right:
                return False
            else:
                return left.val == right.val and is_same(left.left,right.right) and is_same(left.right,right.left)
        
        return is_same(root.left, root.right)