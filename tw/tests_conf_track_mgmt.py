
import unittest
import io
import conf_track_mgmt
from collections import Counter

class ExTest(unittest.TestCase):

  def test_fromat_track(self):
    # morning schedule will finish earlier than 12:00PM
    schedule_am = [(60, 'Talk1'),(60, 'Talk2'), (30, 'Talk3')]
    # afternoon schedule will finish earlier than 04:00PM and contains a lightning talk
    schedule_pm = [(45, 'Talk4'),(60, 'Talk5'), (5, 'Talk6')]
    f = conf_track_mgmt.format_track(schedule_am, schedule_pm)
    expected = ['09:00AM Talk1 60min', '10:00AM Talk2 60min', '11:00AM Talk3 30min', '12:00PM Lunch Time', '01:00PM Talk4 45min', '01:45PM Talk5 60min', '02:45PM Talk6 lightning', '04:00PM Networking Event']
    self.assertEqual(f, expected)

    # afternoon schedule will finish after 04:00PM
    schedule_pm = [(45, 'Talk4'),(120, 'Talk5'), (60, 'Talk6')]
    f = conf_track_mgmt.format_track(schedule_am, schedule_pm)
    expected = ['09:00AM Talk1 60min', '10:00AM Talk2 60min', '11:00AM Talk3 30min', '12:00PM Lunch Time', '01:00PM Talk4 45min', '01:45PM Talk5 120min', '03:45PM Talk6 60min', '04:45PM Networking Event']
    self.assertEqual(f, expected)

  def test_is_perfect_match(self):
    counts = Counter([60,60,30])

    tracks = [[30,30],[60]]
    self.assertFalse(conf_track_mgmt.is_perfect_match(*tracks, counts=counts))
    tracks = [[30], [60],[60]]
    self.assertTrue(conf_track_mgmt.is_perfect_match(*tracks, counts=counts))

  def test_parse_line(self):
    line = 'Some talk 35min'
    self.assertEqual(conf_track_mgmt.parse_line(line), ('Some talk', 35))

    line = 'Smalltalk lightning'
    self.assertEqual(conf_track_mgmt.parse_line(line), ('Smalltalk', 5))

  def test_parse(self):
    s = '''Talk1 30min
Talk number 2 lightning
Talk 3 60min
'''
    d = conf_track_mgmt.parse(io.StringIO(s))
    expected = {'Talk1': 30, 'Talk number 2': 5, 'Talk 3': 60}
    self.assertEqual(d, expected)

  def test_append_lte_slots(self):
    slot_durations = [60,45]
    slots = [60, 60]
    expected = [[60, 60, 60], [60, 60, 45]]
    self.assertEqual(conf_track_mgmt.append_lte_slots(slots, slot_durations), expected)

  def test_is_track_valid(self):
    track = [60,30]
    counts = {60:2, 30:3}
    self.assertTrue(conf_track_mgmt.is_track_valid(track, counts=counts, max_track_duration=120))
    self.assertFalse(conf_track_mgmt.is_track_valid(track, counts=counts, max_track_duration=70),
                      'track is longer than the maximum allowed')

    track = [60,60,60,30]
    self.assertFalse(conf_track_mgmt.is_track_valid(track, counts=counts, max_track_duration=400),
                      'track contains more 60mns slots than available')

  def test_gen_tracks(self):
    def is_track_valid(track):
      return sum(track) <= 90

    c = conf_track_mgmt.gen_tracks([60, 30], is_track_valid)
    expected = [[60, 30], [30, 30], [30, 30, 30]]
    self.assertEqual(c, expected)

  def test_fill_track(self):
    talks = {'Talk1': 30, 'Talk2': 45, 'Talk3': 60}
    am_sch, pm_sch, remaining_talks = conf_track_mgmt.fill_track([45,60],[30], talks)
    self.assertEqual(len(talks), 3, '`talks` should not have been modified')
    self.assertEqual(am_sch, [(45, 'Talk2'),(60,'Talk3')])
    self.assertEqual(pm_sch, [(30, 'Talk1')])

if __name__ == '__main__':
  unittest.main()
