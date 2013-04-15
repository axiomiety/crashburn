from    fabric.api  import env, run, task
from    collections import defaultdict
import  sys
import  os

'''
Invoke with:
fab [localhosts|prod:hostfile=/path/to/file] chkhosts

Data is stored to make it pretty to be displayed, not necessarily to make it easy to parse.
If this were to be used regularly, I'd look into saving the data in an easily parseable format
especially as historical data would come in handy to track down issues

It might also be worth looking into using the @parallel decorator with fabric. Most of the tasks
below are really just waiting for data and the running time of this script would increase
linearly with the number of hosts to be checked. @parallel could help save some time.
'''

# this is where we store all our results
data = defaultdict(dict)

def saveData(host, action, output):
  data[host][action] = output

def prettyPrintData():
  for host in sorted(data.keys()):
    for action in sorted(data[host].keys()):
      print('[%s][%s]\n%s' % (host, action, data[host][action]))

@task
def localhosts():
  env.hosts = ['localhost','freya']

@task
def prod(hostfile='/var/tmp/hosts'):
  print('grabbing hosts from %s' % hostfile)
  with open(hostfile, 'r') as f:
    env.hosts = [h.strip() for h in f.readlines()]
  print('found %s hosts' % len(env.hosts))

# 1. A 30 second sample of number of context switches on the host.
@task
def part_1():
  output = run("sar -w 30 1 | tail -1 | awk '{print $3}'")
  saveData(host=env.host, action='30s ctx switches sample', output=output)

# 2. Whether the node is able to reach each of the other nodes
@task
def part_2():
  hosts_to_ping = [h for h in env.hosts if h is not env.host] # all but ourselves
  msgs = []
  for h in hosts_to_ping:
    # if we don't get a reply within 5s, we consider the host unreachable
    raw_output = run("ping -c 4 -W 5 %s | grep 'packets transmitted'" % h)
    recv =  raw_output.split(',')[1].strip()
    if recv.startswith('0'):
      msgs.append( 'could not reach host %s' % h )
  
  if msgs:
    saveData(host=env.host, action='reachability', output='\n'.join(msgs))

# 3. A 30 second sample of the average service time for each of the physical disks attached to the node.
@task
def part_3():
  raw_output = run("iostat -x 30 1 | awk '{print $1,$13}' | sed -n -e '/^Device/,$p'")
  d = filter(lambda v: len(v) == 2, [x.strip().split(' ') for x in raw_output.split('\n') if x])
  o = '\n'.join(['%s\t%s' % tuple(t) for t in d])
  saveData(host=env.host, action='avg disk service time', output=o)

# 4. The top five processes in total cpu time
@task
def part_4():
  output = run("ps -eo user,pid,comm,%cpu k -%cpu | head -6") # -6 because we want to keep the header
  saveData(host=env.host, action='top 5 cpu intensive procs', output=output)

# 5. The top five processes in resident memory usage
@task
def part_5():
  output = run("ps -eo user,pid,comm,rss,vsz k -rss | head -6")
  saveData(host=env.host, action='top 5 mem intensive procs', output=output)

# 6. The current cpu load of the machine
@task
def part_6():
  #TODO:  we probably want something like loadavg over time
  output = run("cat /proc/loadavg | cut -d' ' -f1")
  saveData(host=env.host, action='cpu load for the last minute', output=output)

# 7. Any retransmits on any of its interfaces
@task
def part_7():
  #TODO: not ideal, as this only looks at the current interval. ideally we'd be looking
  #      at retransmits since the last time those checks were run
  output = run("sar -n ETCP 10 1 | grep ^Average | awk '{print $4}'")
  if output != '0.00':
    saveData(host=env.host, action='checking for retransmits', output='retransmits found: %s' % output)

# 8. The average file size per directory under /home
@task
def part_8():
  home = '/home'
  avg_cmd = "find PATH -ls | awk '{sum += $7; n++;} END {print sum/n;}'"
  dirs = ['/'.join([home, directory]) for directory in os.listdir(home)]
  msgs = []
  for d in dirs:
    o = run(avg_cmd.replace('PATH', d))
    msgs.append('%s\t%s' % (d, o))

  if msgs:
    saveData(host=env.host, action='avg file size in each home dir', output='\n'.join(msgs))

@task
def chkhosts():
  part_1()
  part_2()
  part_3()
  part_4()
  part_5()
  part_6()
  part_7()
  part_8()
  prettyPrintData()
