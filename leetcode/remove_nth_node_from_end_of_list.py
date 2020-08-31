def removeNthFromEnd(head: ListNode, n: int) -> ListNode:
    orig, trail = head, head
    
    for _ in range(n):
        head = head.next
    
    if head is None:
        # we're removing the first element
        return orig.next
    
    while head.next != None:
        trail = trail.next
        head = head.next
    trail.next = trail.next.next
    return orig