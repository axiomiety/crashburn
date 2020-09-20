def intersect(nums1: List[int], nums2: List[int]) -> List[int]:
        from collections import Counter
        c1 = Counter(nums1)
        c2 = Counter(nums2)
        ret = []
        for elem, count in c1.items():
            if elem in c2:
                ret.extend([elem]*min(count, c2[elem]))
        return ret