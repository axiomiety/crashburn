#
# TODO: move the config file to yaml?
# TODO: functionality to show the status of outstanding jobs

import  boto
import  logging
import  base64
import  unittest
import  logging
import  os

REGION_NAME = 'eu-west-1'
CONFIG_FILE = '/tmp/glacier.cred'
RECORD_FILE = '/tmp/glacier.record'
RECORD_FIELDNAMES = ['vault_name', 'archive_id', 'archive_descr', 'upload_utc_timestamp', 'size_kb', 'md5sum']
JOB_LOG = '/tmp/glacier.job'
JOB_FIELDNAMES = ['job_id', 'action', 'starttime_utc_timestamp' , 'status']

class Glacier(object):
  def __init__(self, config_file=CONFIG_FILE, key_id=None, key=None, region_name=REGION_NAME, record_file=RECORD_FILE, job_log=JOB_LOG):
    if key_id is None and key is None:
      logging.info('not enough credentials passed in, sourcing from config file')
      with open(config_file, 'r') as fh:
        cfg     = parse_config_file(fh)
        key_id  = cfg['AWSAccessKeyId']
        key     = cfg['AWSSecretKey']
    region_name       = region_name
    self.record_file  = record_file
    self.job_log      = job_log
    self.glacier      = boto.connect_glacier(aws_access_key_id=key_id, aws_secret_access_key=key, region_name=region_name)

  def upload(self, vault_name, archive, descr=''):
    vault = self.glacier.get_vault(vault_name)
    ts = get_epoc_ts() # we get the timestamp as we start the upload
    archive_id = vault.upload_archive(archive, description=descr)
    b64descr = base64.b64encode(descr)
    with open(self.record_file, 'a+') as fh:
      append_record({ 'vault_name':           vault_name,
                      'archive_id':           archive_id,
                      'archive_descr':        b64descr,
                      'upload_utc_timestamp': ts,
                      'size_kb':              os.path.getsize(archive),
                      'md5sum':               get_md5sum(archive),
                    },
                    fh)

  def get_inventory(self, vault_name):
    vault = self.glacier.get_vault(vault_name)
    job_id = vault.retrieve_inventory()
    log_job(job_id, status='init', ts=get_epoc_ts())

#def log_job(

def get_md5sum(f):
  '''
    We read the file in chunks of 8192 bytes, as otherwise we need to read it whole in memory
    and it can become unmanageable quickly. Cf SO
  '''
  import hashlib
  md5 = hashlib.md5()
  with open(f,'rb') as f: 
    for chunk in iter(lambda: f.read(8192), b''): 
      md5.update(chunk)
  # we return this in hex to match the default output of the md5sum utils on most operatin systems
  return md5.hexdigest() 

def parse_record_file(fh):
  #TODO maybe we can do something nicer with sqlite3 and :memory:
  import csv
  reader = csv.DictReader(fh, fieldnames=RECORD_FIELDNAMES)
  return [row for row in reader]

def append_record(rec, fh):
  '''
    `rec` is a dictionary containing keys matching RECORD_FIELDNAMES
    `fh` is the filehandle for csv file keeping track of the records
  '''
  fh.write(','.join( [str(rec[col]) for col in RECORD_FIELDNAMES] ))
  fh.write('\n') # newline is important!

def parse_config_file(fh):
  '''
    Parses a config file with key=value pairs, one on each line
    Will throw an error if the file does not contain both AWSAccessKeyId and AWSSecretKey
  '''
  c = {}
  for line in fh.readlines():
    k, v = line.strip().split('=')
    c[k] = v
  assert 'AWSAccessKeyId' in c
  assert 'AWSSecretKey' in c
  return c

def get_epoc_ts():
  import datetime, calendar
  d = datetime.datetime.utcnow()                                                                                                                                                                               
  return calendar.timegm(d.timetuple())

class ModuleMethodsTest(unittest.TestCase):

  def tearDown(self):
    #patch.stopAll()
    pass

  def test_parse_record_file(self):
    import StringIO
    f = StringIO.StringIO()
    f.write('AWSAccessKeyId=access_key_id\n')
    f.write('AWSSecretKey=secret_key\n')
    f.write('foo=bar')
    f.seek(0) # we need to rewind the file otherwise readline won't work
    d = parse_config_file(f)
    self.assertEqual( d, {'AWSAccessKeyId':'access_key_id', 'AWSSecretKey':'secret_key', 'foo':'bar'})
    f.close()
    
  def test_append_record(self):
    import StringIO
    f = StringIO.StringIO()
    raw = ['myvault','archiveid','descr_in_base64',1234567890,400,'file_md5sum']
    rec = {k:v for k,v in zip(RECORD_FIELDNAMES, raw)}
    append_record( rec, f )
    self.assertEqual( f.getvalue(), ','.join(str(r) for r in raw) + '\n' )
    f.close()

if __name__ == '__main__':

  import argparse
  parser = argparse.ArgumentParser(description='Process some integers.')
  parser.add_argument('-t', '--test', action='store_true')
  parser.add_argument('-a', '--archive', dest='archive')
  parser.add_argument('-v', '--vault', dest='vault')
  parser.add_argument('-d', '--description', dest='descr')
  parser.add_argument('action', nargs='?', default='upload') # positional
  args = parser.parse_args()

  if args.test:
    import sys
    sys.argv = [sys.argv[0]] # clean up sys.argv otherwise unittest will complain
    #from mock import patch, Mock
    unittest.main()

  else:
    if args.action == 'upload':
      gl = Glacier()
      if args.archive and args.vault:
        descr = args.descr or ''
        gl.upload(args.vault, args.archive, descr=descr)
      else:
        parser.print_help()
    
