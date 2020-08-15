def maxDepth(root: TreeNode) -> int:
    if root is None:
        return 0
    return 1 + max(maxDepth(root.left) if root.left else 0, maxDepth(root.right) if root.right else 0)