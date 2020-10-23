# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    def mergeTwoLists(self, l1: ListNode, l2: ListNode) -> ListNode:
        
        node = ListNode()
        root = node
        while l1 is not None:
            if l2 is not None:
                if l1.val > l2.val:
                    node.next = l2
                    l2 = l2.next
                else:
                    node.next = l1
                    l1 = l1.next
            else:
                node.next = l1
                l1 = l1.next
            node = node.next
        while l2 is not None:            
            if l1 is not None:
                if l1.val > l2.val:
                    node.next = l2
                    l2 = l2.next
                else:
                    node.next = l1
                    l1 = l1.next
            else:
                node.next = l2
                l2 = l2.next
            node = node.next
        return root.next