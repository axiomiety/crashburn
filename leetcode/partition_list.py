class Solution:
    def partition(self, head: Optional[ListNode], x: int) -> Optional[ListNode]:
        
        if not head:
            return
        
        new_head = ListNode()
        new_head_pivot = ListNode()
        
        node = head
        node_left, node_right = new_head, new_head_pivot
        while node:            
            if node.val < x:
                node_left.next = node
                node_left = node
            else:
                node_right.next = node
                node_right = node
            node = node.next
        
        # now link the 2
        node_left.next = new_head_pivot.next
        node_right.next = None
        
        return new_head.next
