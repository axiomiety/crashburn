# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    def rotateRight(self, head: ListNode, k: int) -> ListNode:
        
        def find_length_tail(node):
            count = 1
            while node.next:
                count += 1
                node = node.next
            return count, node
        
        if head is None:
            return head
        length, tail = find_length_tail(head)
        
        # no-op
        if k == 0 or length == 1 or k % length == 0:
            return head
        
        # find the new position, mod length
        new_head_pos = length-(k%length)
        
        if new_head_pos:
            node, prev = head, None
            for _ in range(new_head_pos):
                prev = node
                node = node.next
            # new head
            new_head = node
            # set the tail
            prev.next = None
            # link the current tail
            tail.next = head
            return new_head
        else:
            return head