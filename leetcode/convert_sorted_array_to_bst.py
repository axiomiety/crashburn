def sortedArrayToBST(nums: List[int]) -> TreeNode:
    def mktree(arr):
        if not arr:
            return None
        n = len(arr)
        if n == 1:
            return TreeNode(val=arr[0], left=None, right=None)
        
        midIdx = n//2
        return TreeNode(val=arr[midIdx], left=mktree(arr[0:midIdx]), right=mktree(arr[midIdx+1:]))
    return mktree(nums)