def reverse_str(self, x: int) -> int:
    sign = 1 if x >= 0 else -1
    x *= sign
    val = int(''.join(reversed(str(x)))) * sign
    return 0 if val >= 2**31 or val <= -2**31 else val

def reverse(x: int) -> int:
    ret = 0
    sign = 1 if x >=0 else -1
    x *= sign
    while x > 0:
        digit = x % 10
        ret *= 10
        ret += digit
        x = x//10
    return ret*sign if ret < 2**31 else 0