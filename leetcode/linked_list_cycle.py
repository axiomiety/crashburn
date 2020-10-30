class Solution:
    def hasCycle(self, head: ListNode) -> bool:
        
        ptrA = head
        if head and head.next and head.next.next:
          ptrB = head.next.next
        else:
          return False  
        while ptrA is not None and ptrB is not None:
            if ptrA == ptrB:
                return True
            else:
                ptrA = ptrA.next
                if ptrB.next and ptrB.next.next:
                    ptrB = ptrB.next.next
                else:
                    return False
        return False