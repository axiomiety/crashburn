class Solution:
    def fizzBuzz(self, n: int) -> List[str]:
        ret = []
        for i in range(1, n+1):
            val = ""
            if i % 3 == 0:
                val += "Fizz"
            if i % 5 == 0:
                val += "Buzz"
            if val == "":
                val = str(i)
            ret.append(val)
        
        return ret