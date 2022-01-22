class Solution:
    def restoreIpAddresses(self, s: str) -> List[str]:
        
        def combine(digit, sections):
            if not sections:
                return [[digit]]
            ret = []
            for section in sections:
                ret.append([digit,*section])
                head = section[0]
                number = f"{digit}{head}"
                
                if int(number) <= 255:
                    ret.append([number,*section[1:]])
        
            return [c for c in ret if len(c) <= 4]
    
        sections = []
        for digit in reversed(s):
            sections = combine(digit, sections)
            # dead end
            if not sections:
                return []
        
        ips = set()
        for section in sections:
            if len(section) != 4 or any(len(c) >= 2 and c[0] == "0" for c in section):
                continue
            ips.add(".".join(c for c in section))
        return list(ips)