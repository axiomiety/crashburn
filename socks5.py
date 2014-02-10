
# sources:
# http://www.ietf.org/rfc/rfc1928.txt

VER       = 0x05 # SOCKS v5
NMETHODS  = 1 # 1 octet == 1 byte
RSV       = 0x00 # reserved

# 0x03 to 0x7f: IANA assigned
# 0x80 to 0xfe: Reserved for private methods
METHOD = {
  0x00: 'No authentication required',
  0x01: 'GSSAPI',
  0x02: 'Username/Password',
  0xff: 'No acceptable methods',
}

CMD = {
  0x01: 'CONNECT',
  0x02: 'BIND',
  0x03: 'UDP ASSOCIATE',
}

ATYP = {
  0x01: 'IPv4',
  0x03: 'Domain name',
  0x04: 'IPv6',
}

# 0x09 to 0xff are unassigned
REP = {
  0x00: 'Succeeded',
  0x01: 'General SOCKS server failure',
  0x02: 'Connection not allowed by ruleset',
  0x03: 'Network unreachable',
  0x04: 'Host unreachable',
  0x05: 'Connection refused',
  0x06: 'TTL expired',
  0x07: 'Command not supported',
  0x08: 'Address type not supported',
}

from collections import namedtuple

class Comms(object):
  CLIENT_INIT = namedtuple('CLIENT_INIT', ['ver', 'nmethods', 'method'])
  SERVER_INIT_RESP = namedtuple('SERVER_INIT_RESP', ['ver', 'method'])
  # method-specific sub-negotiation
  
  CLIENT_REQUEST = namedtuple('CLIENT_REQUEST', ['ver', 'cmd', 'rsv', 'atyp', 'dst_addr', 'dst_port', 'data'])
  SERVER_RESPONSE = namedtuple('SERVER_RESPONSE', ['ver', 'rep', 'rsv', 'atyp', 'bnd_addr', 'bnd_port', 'data'])

  @staticmethod
  def parse(c, data):
    pass

import socket
import threading
import socketserver
import struct

class ThreadedTCPRequestHandler(socketserver.BaseRequestHandler):
  allow_reuse_address = True
  init = False

  def _handshake(self):
    ver = struct.unpack('!B', self.request.recv(1))[0]
    nmethods = struct.unpack('!B', self.request.recv(1))[0]
    methods = struct.unpack('!' + 'B'*nmethods, self.request.recv(nmethods))[0]
    print('SOCKS => ver=%s; nmethods=%s; methods=%s' % (ver, nmethods, METHOD[methods]))
    response = b'\x05\x00' # socks v5, auth method 0 - no auth
    self.request.sendall(response)
    ThreadedTCPRequestHandler.init = True

  def getAddrPortPair(self, atyp):
    if atyp == 0x01:
      addr = socket.inet_ntoa(self.request.recv(4))
    elif atyp == 0x03:
      num_octets = struct.unpack('!B', self.request.recv(1))[0]
      addr = self.request.recv(num_octets)
    elif atyp == 0x04:
      addr = socket.inet_ntop(socket.AF_INET6, self.request.recv(16))
    else:
      # raise some exception!
      pass

    port = struct.unpack('!H', self.request.recv(2))[0]
    return (addr, port)

  def handle(self):
    if not ThreadedTCPRequestHandler.init:
      self._handshake()
    data = self.request.recv(4)
    (ver, cmd, rsv, atyp) = struct.unpack('!BBBB', data)
    print('SOCKS => ver=%s; cmd=%s; rsv=%s; atyp=%s' % (ver, CMD[cmd], rsv, ATYP[atyp]))
    addr, port = self.getAddrPortPair(atyp)
    print('addr=%s;port=%s' % (addr, port))

class ThreadedTCPServer(socketserver.ThreadingMixIn, socketserver.TCPServer):
  pass

def main():
  HOST, PORT = 'localhost', 8123

  server = ThreadedTCPServer((HOST, PORT), ThreadedTCPRequestHandler)
  ip, port = server.server_address

  # Start a thread with the server -- that thread will then start one
  # more thread for each request
  server_thread = threading.Thread(target=server.serve_forever)
  # Exit the server thread when the main thread terminates
  server_thread.daemon = False
  server_thread.start()
  
  #server.shutdown()
  
if __name__ == '__main__':
  main()

########
# testing via curl:
# curl --proxy socks5h://localhost:8123 http://
#
# nc -l -p 8123 -o /var/tmp/nc_pkt_dump
# ip_header = '3c20303030303030303020303520303220303020'
# binascii.unhexlify(bytes(ip_header.encode('ascii')))
# b'< 00000000 05 02 00 '
