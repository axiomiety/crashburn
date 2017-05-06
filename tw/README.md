# Preamble

After reading through the 3 sets, I originally settled on the Merchant's Guide to the Galaxy due to time constraints. However after completing it I thought I'd given the Conference Track Management a go - we're organising an internal 'DevCon' conference and it felt like something I could re-use (and it turned out to be more fun too!).

# Conference Track Management

## TL;DR

A sample list of talks called `sample_talks.txt` has been provided. Input should be of a similar format. You can run it as follows:

```sh
$ python3 conf_track_mgmt.py sample_talks.txt
Track 1
09:00AM Rails Magic 60min
10:00AM Ruby on Rails: Why We Should Move On 60min
11:00AM Ruby on Rails Legacy App Maintenance 60min
12:00PM Lunch Time
01:00PM Accounting-Driven Development 45min
01:45PM Pair Programming vs Noise 45min
02:30PM Common Ruby Errors 45min
03:15PM Clojure Ate Scala (on my project) 45min
04:00PM Ruby Errors from Mismatched Gem Versions 45min
04:45PM Networking Event
Track 2
09:00AM Communicating Over Distance 60min
10:00AM Writing Fast Tests Against Enterprise Rails 60min
11:00AM User Interface CSS in Rails Apps 30min
11:30AM A World Without HackerNews 30min
12:00PM Lunch Time
01:00PM Overdoing it in Python 45min
01:45PM Ruby vs. Clojure for Back-End Development 30min
02:15PM Woah 30min
02:45PM Sit Down and Write 30min
03:15PM Lua for the Masses 30min
03:45PM Programming in the Boondocks of Seattle 30min
04:15PM Rails for Python Developers lightning
04:20PM Networking Event
```

Note your output may differ due to the (somewhat) non-deterministic nature of the algorithm.

## Running the tests

A set of tests have been provided in `tests_conf_track_mgmt.py` and are run as follows:
```sh
$ python3 tests_conf_track_mgmt.py
........
----------------------------------------------------------------------
Ran 8 tests in 0.005s

OK
```

## Design considerations

Where possible the functions provided have no side effects and do not rely on internal state - i.e. they are pure & functional.

The schedule generation algorithm yields one solution at a time as opposed to providing *all* possible solutions upfront (which could potentially take very long).

The original problem can be simplified (with no loss of generality) to limiting ourselves to ordered tracks. That is, a morning session of `45, 60, 30` minutes slots is equivalent to `60, 45, 30`. This helps generate solutions faster and the user is then free to shuffle slots within sessions, without impacing the overall schedule.

## Here be dragons

* `gen_slots` fixes the morning track to `max_am_track_duration` - this limits the number of possibilities but yields solutions faster. This restriction could be toggled with a flag
* Extending the schedule algorithm to work over `n` days should be relatively easy (famous last words) but has *not* been tested. At *all*. Caveat emptor. Really.
* `format_track` isn't particularly pretty - I ran out of time to refactor this nicely : (
* If there is a gap, e.g. between a 30mns session at 11:00 and lunch at noon, this is not indicated (as per the original requirements). It would be clearer to add something like 'Break' to fill in the gaps.

