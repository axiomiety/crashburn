def longestCommonPrefix(strs: List[str]) -> str:
    if not strs:
        return ""
    ret = []
    for i in range(len(strs[0])):
        # take the next letter in the first word
        letter = strs[0][i]
        for word in strs:
            if i < len(word) and letter == word[i]:
                continue
            else:
                # our index is greater, so we return what we have
                return ''.join(ret)
        ret.append(letter)
    # we exhausted all the letters in the first word       
    return ''.join(ret)