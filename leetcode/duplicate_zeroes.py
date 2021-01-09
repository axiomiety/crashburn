from typing import List
def duplicateZeros_1(arr: List[int]) -> None:
    """
    Do not return anything, modify arr in-place instead.

    It's a doubly linked list under the covers so insert and pop (from the end)
    are relatively cheap
    """
    idx = 0
    arr_size = len(arr)
    while idx < arr_size:
        if arr[idx] == 0:
            arr.pop()
            arr.insert(idx+1,0)
            idx += 1
        idx += 1

def duplicateZeros_2(arr: List[int]) -> None:
    max_len = len(arr)
    count, max_idx = 0, 0
    copy_to = len(arr) - 1

    for idx, val in enumerate(arr):
        count += 1
        if val == 0:
            count += 1
        if count == max_len:
            max_idx = idx
            break
        if count > max_len:
            # that means a zero at the boundary!
            arr[max_len-1] = 0
            copy_to -= 1
            max_idx = idx-1
    
    while max_idx >= 0:
        # start copying backwards
        arr[copy_to] = arr[max_idx]
        copy_to -= 1
        if arr[max_idx] == 0:
            arr[copy_to] = 0
            copy_to -= 1
        max_idx -= 1


if __name__ == '__main__':
    #arr = [1,0,2,3,0,4,5,0]
    arr = [8,4,5,0,0,0,0,7]
    #     [8,4,5,0,0,0,0,0]
    duplicateZeros_2(arr)
    print(arr)
    assert arr == [8,4,5,0,0,0,0,0]
