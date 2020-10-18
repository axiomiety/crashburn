class Solution:
    def isPalindrome(self, head: ListNode) -> bool:
        def length(node):
            n = 0
            while node is not None:
                n += 1
                node = node.next
            return n

        n = length(head)
        if n == 0 or n == 1:
            return True
     
        mid = n//2
        
        # populate a stack up to the mid-point of the list
        stack = []
        i = 0
        node = head
        while i < mid:
            stack.append(node.val)
            i += 1
            node = node.next
        
        def get_ith_node(node, i):
            j = 0
            while j < i:
                node = node.next
                j += 1 
            return node
        
        node = get_ith_node(head, math.ceil(n/2.0))
        # pop from the stack & compare one by one
        while node is not None:
            val = stack.pop()
            if val != node.val:
                return False
            node = node.next
        return True