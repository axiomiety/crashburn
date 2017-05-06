import io

from collections import Counter
from functools import partial
from itertools import permutations, product, chain
from datetime import datetime, time, timedelta

TIME_FMT = '%I:%M%p'

def format_track(schedule_am, schedule_pm):
  '''
    Python does not allow timedelta to work with time - but we do not *currently* care about
    the date so we use a datetime for calculations, but stip the date component in the output.

    The logic for the lunch hour and networking event is hardcoded. This could be extracted.
  '''
  ret = []
  # start time for the conference
  t = datetime.combine(datetime.today(), time(9,0))

  def fmt_duration(duration):
    return 'lightning' if duration == 5 else '{0}min'.format(duration)

  def fmt_line(duration, event):
    return '{time} {event} {duration}'.format(time=t.strftime(TIME_FMT),
                                              event=event,
                                              duration=fmt_duration(duration))

  for duration, event in schedule_am:
    ret.append(fmt_line(duration, event))
    t += timedelta(minutes=duration)
  
  # lunch time starts at noon
  lunch_time = datetime.combine(datetime.today(), time(12,0))
  ret.append('{time} {event}'.format(time=lunch_time.strftime(TIME_FMT), event='Lunch Time'))
  t = lunch_time + timedelta(hours=1)

  for duration, event in schedule_pm:
    ret.append(fmt_line(duration, event))
    t += timedelta(minutes=duration)

  networking_min_time = datetime.combine(datetime.today(), time(16,0))
  networking_time = max(t, networking_min_time)
  ret.append('{time} {event}'.format(time=networking_time.strftime(TIME_FMT), event='Networking Event'))

  return ret

def gen_slots(d, max_am_track_duration=180, max_pm_track_duration=240, num_days=2):
  '''
    yields a series of slot to fit in all the given talks, separated into morning and afternoon
  '''
  values = d.values()
  counts = Counter(values)
  max_duration = max(max_am_track_duration, max_pm_track_duration)
  itv = partial(is_track_valid, counts=counts, max_track_duration=max_duration)

  potential_tracks = gen_tracks(counts.keys(), is_track_valid=itv)

  #TODO we could add a flag to see whether we want the morning track to be <= max_am_track_duration? though it'll take longer to find a valid solution
  p1 = permutations([t for t in potential_tracks if sum(t) == max_am_track_duration], num_days)
  p2 = permutations([t for t in potential_tracks if max_pm_track_duration >= sum(t)], num_days)

  yield next(list(zip(am,pm)) for am, pm in product(p1, p2) if is_perfect_match(*am, *pm, counts=counts))
  
  
def is_perfect_match(*args, counts=None):
  '''
    given a series of tracks, returns true if all available talks can be scheduled
  '''
  c = Counter(chain.from_iterable(args))
  for duration, count in counts.items():
    if count != c[duration]:
      return False
  return True

def is_track_valid(track, counts=None, max_track_duration=180):
  '''
    a track is valid if and only if its runtime is less than the allowed maximum,
    and its slots (e.g. 3x60) do not exceed the available slots
  '''
  runtime = sum(track)
  if runtime > max_track_duration:
    return False
  for duration, count in Counter(track).items():
    if count > counts[duration]:
      return False
  return True

def gen_tracks(slot_durations, is_track_valid):
  '''
    generates tracks which satisfy the `is_track_valid` predicate
    tracks are time-bound, nor order-bound - so a [60,60,30] track is equivalent to [30,60,60]
  '''
  slots_added = True
  candidates = []
  new_slots = [[o] for o in slot_durations] 
  while slots_added:
    temp = []
    for candidate in new_slots:
      temp.extend(append_lte_slots(candidate, slot_durations))
    temp = [s for s in temp if is_track_valid(s)]
    if temp:
      candidates.extend(temp[:])
      new_slots = temp[:]
    else:
      slots_added = False
  return candidates

def append_lte_slots(slots, slot_durations):
  # slots are ordered so the last element is our point of reference
  lte_slots = [slot for slot in slot_durations if slot <= slots[-1]]
  ret = []
  for slot in lte_slots:
    ret.append(slots[:] + [slot])
  return ret

def parse_line(line):
  '''
    Turns 'Some cook talk 50min' into a ('Some cool talk', 50) tuple
  '''
  tokens = line.split()
  title = ' '.join(tokens[:-1])
  duration = tokens[-1]
  numeric_duration = 0
  if duration.endswith('min'):
    numeric_duration = int(duration[:-3])
  elif duration == 'lightning':
    numeric_duration = 5
  return title, numeric_duration

def parse(f):
  '''
    f is a stream
  '''
  d = {}
  for line in f:
    title, duration = parse_line(line)
    d[title] = duration

  return d

def pick_talk_given_duration(talks, duration):
  '''
    given a duration (e.g. 60), will return a (somewhat random) talk of that length.
    we assume such a talk is available (which it should be by design!)
  '''
  for talk, d in talks.items():
    if d == duration:
      return talk

def fill_track(am_slots, pm_slots, talks):
  '''
    will (somewhat randomly) assign a talk of a required duration to a slot
  '''
  t = talks.copy()
  am_sch = []
  pm_sch = []
  for slots, schedule in [(am_slots, am_sch), (pm_slots, pm_sch)]:
    for slot in slots:
      talk = pick_talk_given_duration(t, slot)
      schedule.append((slot, talk))
      del t[talk]
  return (am_sch, pm_sch, t)

if __name__ == '__main__':
  import argparse
  parser = argparse.ArgumentParser()
  parser.add_argument('fname',help='name of the file containing a list of talks and their duration')
  args = parser.parse_args()

  with open(args.fname) as f:
    talks = parse(f)
    #TODO we only generate a single schedule - we could generate more than one if required
    for idx, track in enumerate(next(gen_slots(talks))):
      print('Track {0}'.format(idx+1))
      am, pm, remaining_talks = fill_track(*track, talks)
      print('\n'.join(format_track(am, pm)))
      talks = remaining_talks
