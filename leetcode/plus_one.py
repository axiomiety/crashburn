from typing import List

def plusOne(digits: List[int]) -> List[int]:
    # simple case, no carry
    if digits[-1] < 9:
        digits[-1] += 1
        return digits

    # we know the last digit is 9
    digits.reverse() # in-place
    carry = True
    for idx, val in enumerate(digits):
        if carry:
            if val == 9:
                digits[idx] = 0
            else:
                digits[idx] = val + 1
                carry = False
        else:
            break
    if carry:
        digits.append(1)
    digits.reverse()
    return digits

if __name__ == '__main__':
    print(plusOne([9,9]))