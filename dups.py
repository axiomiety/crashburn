import sys
from operator import itemgetter
import getopt
import os.path
from collections import defaultdict
import md5

# REFERENCES:

def usage():
  print "-d directory"

def main():
  print "dups - find duplicates in a given directory, or pair of directories"

  try:
    opts, args = getopt.getopt(sys.argv[1:], "hd:", ["help","directory="])
  except getopt.GetoptError, err:
    print str(err) # will print something like "option -a not recognized"
    usage()
    sys.exit(2)
  
  mainDir = None
  for o, a in opts:
    if o == "-h":
      usage()
      sys.exit(1)
    elif o in ("-d", "--directory"):
      mainDir = a

  if None == mainDir:
    logmsg("a directory needs to be supplied", LogLevel.ERR)

  ddict = processDir(mainDir)
  digDeeper(ddict)

def digDeeper(ddict):
  for sz,files in ddict.items():
    if len(files) > 2:
#      print "following files have the same size (%d bytes):" % k
      dh = defaultdict(list)
      for f in files:
        dh[md5hash(f)].append(f)

      for md5s, files in dh.items():
        if len(files) > 2:
          logmsg("following files have the same md5sum - they are identical", LogLevel.INFO)
          for f in files:
            print f

def md5hash(f):
  ff = file(f,'rb')
  digest = md5.new(ff.read()).hexdigest()
  return digest

def processDir(d):
  if not os.path.isdir(d):
    logmsg("%s is not a directory!" % d, LogLevel.ERR)
    sys.exit(2)

  ddict = defaultdict(list)

  for root, dirs, files in os.walk(d):
    for f in files:
      path = os.path.join(root,f)
      ddict[os.stat(path).st_size].append(path)
  
  return ddict

def logmsg(msg, level):
  if (LogLevel.ERR == level):
    print "[err] %s" % msg
  elif (LogLevel.INFO == level):
    print "[info] %s" % msg

class LogLevel:
  ERR=1
  INFO=2

if __name__ == "__main__":
  main()

