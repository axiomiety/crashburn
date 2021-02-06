class Solution:
    def sortArrayByParity(self, A: List[int]) -> List[int]:
        
        if not A: # empty array
            return A
        
        left = 0
        right = len(A)-1
        
        is_even = lambda a: a%2 == 0
        is_odd = lambda a: not is_even(a)
        
        def swap(a, b):
            tmp = A[a]
            A[a] = A[b]
            A[b] = tmp
        
        while left < right:
            if is_even(A[left]):
                left += 1
            else:
                if is_odd(A[right]):
                    right -= 1
                else:
                    swap(left, right)
                    left += 1
                    right -= 1
        return A