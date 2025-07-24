

class SearchGrid:
    grid: list[list[set[int]]]
    n: int
    def __init__(self, n:int):
        self.grid = []
        self.n = n
        for row in range(n):
            self.grid.append([])
            for _ in range(n):
                self.grid[row].append(set())
    
    def add_to_row(self, row:int, digits: tuple[set[int],...]) -> None:
        for col_idx, set_of_digits in enumerate(sets_of_digits):
            self.grid[row][col_idx].append(set_of_digits)

    def add_to_col(self, col:int, digits: tuple[set[int],...]) -> None:
        for row_idx, set_of_digits in enumerate(sets_of_digits):
            self.grid[row_idx][col].append(set_of_digits)


    def intersect(self):
        for row in range(self.n):
            for col in range(self.n):
                self.grid[row][col] = set.intersection(*self.grid[row][col])


def to_digits(val: int, n: int) -> list[int]:
    """
    expands val into individual digits

    returns a list of length n, 0-padded
    """
    ret = []
    x = val
    while x > 0:
        x, digit = divmod(x, 10)
        ret.append(digit)

    while len(ret) < n:
        ret.append(0)

    return list(reversed(ret))

def from_digits(arr: list[int]) -> int:
    return sum(digit*10**(idx) for idx, digit in enumerate(reversed(arr)))

def to_grid(digits: list[int], n: int) -> list[list[int]]:
    return [to_digits(val,n) for val in digits]

def pretty_print_grid(grid: list[list[int]]) -> None:
    for row in grid:
        print(",".join(str(col) for col in row))
    
def is_valid(digits: list[int], n: int) -> bool:
    for idx, number in enumerate(digits):
        if number % (idx+1) != 0:
            return False

    g = to_grid(digits, n)
    # transpose the grid
    g_t = [list(row) for row in zip(*g)]
    for idx, number in enumerate(from_digits(row) for row in g_t):
        if number % (idx+1+n) != 0:
            return False

    return True

def is_valid_t(digits: list[int], n: int) -> bool:
    for idx, number in enumerate(digits):
        if number % (idx+1+n) != 0:
            return False

    g = to_grid(digits, n)
    # transpose the grid
    g_t = [list(row) for row in zip(*g)]
    for idx, number in enumerate(from_digits(row) for row in g_t):
        if number % (idx+1) != 0:
            return False

    return True


def test_to_digits() -> None:
    assert to_digits(123,3) == [1,2,3]
    assert to_digits(123,4) == [0,1,2,3]

def test_from_digits() -> None:
    assert from_digits([1,2,3]) == 123
    assert from_digits([0,1,2,3]) == 123

def test_to_grid() -> None:
    assert to_grid([12,3],n=2) == [[1,2],[0,3]]

def test_is_valid() -> None:
    assert is_valid([16235,52460,4893,24868,47030], 5)
    assert is_valid([99,96], 2)

def test_is_valid_t() -> None:
    assert is_valid_t([15024,62447,24880,36963,50380], 5)
digits = [16235,52460,4893,24868,47030]
pretty_print_grid(to_grid(digits, 5))
is_valid(digits, 5)

def multiples(k: int, n:int) -> list[int]:
    ret = [0]
    val = k
    while val < 10**n:
        ret.append(val)
        val += k
    return list(reversed(ret))
import math

def calc_num_possibilities(n:int) -> int:
    max_value = 10**n-1
    total = 1
    for i in range(n+1,2*n+1):
        # +1 'cause 0
        total *= max_value//i+1
    return total

s = 1
for i in range(6,11):
    x = len(multiples(i,5))
    s *= x
    print(i, x, s)
print(s)
print(f"{calc_num_possibilities(5):>50,}")
print(f"{10**(5*5):>50,}")

def get_ordered_multiples(k:int, n:int) -> list[int]:
    val = 0
    max_value = 10**n
    mults = []
    while val < max_value:
        mults.append((val, sum(to_digits(val,n=n))))
        val += k
    # now sort it out
    return [x for x, _ in sorted(mults, key=lambda x: x[1], reverse=True)]

om4=get_ordered_multiples(4,2)
om3=get_ordered_multiples(3,2)
print(om4)
print(om3)
from itertools import product

def do3() -> None:
    n=3
    om4=get_ordered_multiples(4,n)
    om5=get_ordered_multiples(5,n)
    om6=get_ordered_multiples(6,n)
    
    max_sum = 0
    best_solution = None

    for a in om6:
        for b in om5:
            for c in om4:
                # remember we start with the right-most first
                triad = [c,b,a]
                g_t = [to_digits(x,n) for x in triad]
                g = [list(row) for row in zip(*g_t)]
                s = sum(v for x in g_t for v in x)
                if s < max_sum:
                    break
                if is_valid([from_digits(x) for x in g], n):
                    max_sum = s
                    best_solution = triad
                    print(max_sum, best_solution)
                
def do4() -> None:
    n=4
    om5=get_ordered_multiples(5,n)
    om6=get_ordered_multiples(6,n)
    om7=get_ordered_multiples(7,n)
    om8=get_ordered_multiples(8,n)
    
    max_sum = 0
    best_solution = None

    for a in om8:
        da = to_digits(a,n)
        if sum(da) + 9*n*3 < max_sum:
            break
        for b in om7:
            db = to_digits(b,n)
            if sum(da) + sum(db) + 9*n*2 < max_sum:
                break
            for c in om6:
                dc = to_digits(c,n)
                if sum(da) + sum(db) + sum(dc) + 9*n < max_sum:
                    break
                for d in om5:
                    # remember we start with the right-most first
                    triad = [d,c,b,a]
                    g_t = [to_digits(x,n) for x in triad]
                    g = [list(row) for row in zip(*g_t)]
                    s = sum(v for x in g_t for v in x)
                    if s < max_sum:
                        break
                    if is_valid([from_digits(x) for x in g], n):
                        max_sum = s
                        best_solution = triad
                        print(max_sum, best_solution)
                
def do5() -> None:
    n=5
    om6=get_ordered_multiples(6,n)
    om7=get_ordered_multiples(7,n)
    om8=get_ordered_multiples(8,n)
    om9=get_ordered_multiples(9,n)
    om10=get_ordered_multiples(10,n)
    
    max_sum = 100
    best_solution = None

    for a in om10:
        print(a)
        da = to_digits(a,n)
        if sum(da) + 9*n*4 < max_sum:
            break
        for b in om9:
            db = to_digits(b,n)
            if sum(da) + sum(db) + 9*n*3 < max_sum:
                break
            for c in om8:
                dc = to_digits(c,n)
                if sum(da) + sum(db) + sum(dc) + 9*n*2 < max_sum:
                    break
                for d in om7:
                    dd = to_digits(d,n)
                    if sum(da) + sum(db) + sum(dc) + sum(dd) + 9*n < max_sum:
                        break
                    for e in om6:
                        # remember we start with the right-most first
                        de = to_digits(e,n)
                        g_t = [de,dd,dc,db,da]
                        g = [list(row) for row in zip(*g_t)]
                        s = sum(v for x in g_t for v in x)
                        if s < max_sum:
                            break
                        if is_valid([from_digits(x) for x in g], n):
                            max_sum = s
                            best_solution = g
                            print(max_sum, best_solution)
                    
print("starting...\n")
do5()
