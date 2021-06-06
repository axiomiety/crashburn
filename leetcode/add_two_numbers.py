class Solution:
    def addTwoNumbers(self, l1: ListNode, l2: ListNode) -> ListNode:
        from itertools import zip_longest
        node = ListNode(val=None)
        head = node
        carry = 0
        while l1 or l2:
            a = l1.val if l1 else 0
            b = l2.val if l2 else 0
            val = (a+b)+carry
            carry = 0
            if val > 9:
                carry = 1
                val -=10
            node.next = ListNode(val=val)
            node = node.next
            l1 = l1.next if l1 else None
            l2 = l2.next if l2 else None
        if carry:
            node.next = ListNode(val=carry)
        return head.next