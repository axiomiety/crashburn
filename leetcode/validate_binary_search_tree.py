class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

B = TreeNode(val=5)
D = TreeNode(val=6)
E = TreeNode(val=20)
C = TreeNode(val=15,left=D,right=E)
A = TreeNode(val=10,left=B,right=C)

BB = TreeNode(val=1)
DD = TreeNode(val=3)
EE = TreeNode(val=6)
CC = TreeNode(val=4, left=DD, right=EE)
AA = TreeNode(val=5, left=BB, right=CC)

FF = TreeNode(val=1, left=BB)

def isValidBST(self, root: TreeNode) -> bool:       
    if root is None:
        return True

    q = []
    def visit(node):
        if node.left:
            visit(node.left)
        q.append(node.val) 
        if node.right:
            visit(node.right)
    
    visit(root)
    for i in range(1,len(q)):
        if q[i-1] >= q[i]:
            return False
        
    return True