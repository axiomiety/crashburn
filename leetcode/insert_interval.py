class Solution:
    def insert(self, intervals: List[List[int]], newInterval: List[int]) -> List[List[int]]:

        def process(interval,merged):
            start_interval, end_interval = interval
            start_last, end_last = merged[-1]
            if start_last > end_interval:
                merged.insert(-1, interval)
            elif start_interval > end_last:
                merged.append(interval)
            else:
                merged[-1] = min(start_interval, start_last), max(end_interval, end_last)
            
        
        merged = [newInterval[:]]
        for interval in intervals:
            process(interval, merged)
            
        return merged
    # went about this the wrong way and doubled down when really i shoult have taken a step back
    def carxzyInsert(self, intervals: List[List[int]], newInterval: List[int]) -> List[List[int]]:
        ret = []
        newStart, newEnd = newInterval
        inserted = False
        iterator = iter(intervals)
        while interval := next(iterator, None):
            if inserted:
                ret.append(interval)
            else:
                start, end = interval
                if newEnd < start:
                    ret.append(newInterval)
                    ret.append(interval)
                    inserted = True
                elif newStart > end:
                    ret.append(interval)
                elif newEnd <= end:
                    # we found an enclosing interval
                    ret.append([min(start,newStart),end])
                    inserted = True
                else:
                    # we overlap on the start but not the end
                    overlapStart = min(start, newStart)
                    while overlapInterval := next(iterator, None):
                        overlapIntervalStart, overlapIntervalEnd = overlapInterval
                        if overlapIntervalStart > newEnd:
                            ret.append([overlapStart, newEnd])
                            ret.append(overlapInterval)
                            inserted = True
                            break
                        elif newEnd >= overlapIntervalEnd:
                            continue
                        else:
                            ret.append([overlapStart, overlapIntervalEnd])
                            inserted = True
                            break
                    else:
                        # reached the end
                        ret.append([overlapStart, newEnd])
                        inserted = True
        if not inserted:
            ret.append(newInterval)
        return ret