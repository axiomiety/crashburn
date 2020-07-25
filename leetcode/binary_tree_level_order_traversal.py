def levelOrder(root: TreeNode) -> List[List[int]]:
    ret = []
    nodes_per_level = [[root]]
    while nodes_per_level:
        nodes_to_visit = nodes_per_level.pop(0)
        level = []
        next_level = []
        for node in nodes_to_visit:
            if node: # really for the case of root being empty
                level.append(node.val)
                if node.left:
                    next_level.append(node.left)
                if node.right:
                    next_level.append(node.right)
        if level: # don't append empty lists
            ret.append(level)
        if next_level:
            nodes_per_level.append(next_level)
    return ret