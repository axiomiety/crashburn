def merge(nums1, n, nums2, m):
    q = len(nums1)-1
    def push(k):
        for i in range(q, k, -1):
            nums1[i] = nums1[i-1]
    
    n1idx = 0
    n2idx = 0
    mCount = 0
    while n2idx < n and mCount < m:
        print(f"{n1idx},{n2idx},{nums1}")
        n1 = nums1[n1idx]
        n2 = nums2[n2idx]
        if n1 >= n2:
            push(n1idx)
            nums1[n1idx] = n2
            n2idx += 1
        else:
            mCount += 1
        n1idx += 1
    if n2idx != n:
        while n2idx < n:
            print(f"> {n1idx},{n2idx},{nums1}")
            nums1[n1idx] = nums2[n2idx]
            n2idx += 1
            n1idx += 1
    print(nums1)