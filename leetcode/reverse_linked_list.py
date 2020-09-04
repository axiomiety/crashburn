# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    @staticmethod
    def push(head, node):
        node.next = head.next
        head.next = node
        
    def reverseList(self, head: ListNode) -> ListNode:
        rev_head = ListNode()
        while head != None:
            next_node = head.next
            Solution.push(rev_head, head)
            head = next_node
        return rev_head.next