class Solution:
    def findSubsequences(self, nums: List[int]) -> List[List[int]]:
        sequences = set([()])
        for number in nums:
            new_sequences = [(*seq,number) for seq in sequences if (seq and number >= seq[-1]) or not seq]
            sequences.update(new_sequences)
            
        return [list(seq) for seq in sequences if len(seq) > 1]