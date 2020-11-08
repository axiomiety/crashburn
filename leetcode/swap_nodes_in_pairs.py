# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    def swapPairs(self, head: ListNode) -> ListNode:
        if head and head.next:
            new_head = head.next
            rest = new_head.next
            new_head.next = head
            head.next = self.swapPairs(rest)
            return new_head
        return head
        