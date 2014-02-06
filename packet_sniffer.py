###
# sources
# http://stackoverflow.com/questions/6067405/python-sockets-enabling-promiscuous-mode-in-linux

# NOTE:
# when dealing with ctypes, "" is a string and '' is of type str. they're not compatible

import  socket
import  fcntl
import  ctypes

BUFF_SIZE = 65565

###
# flags

# linux/if_ether.h
ETH_P_ALL     = 0x0003
ETH_P_IP      = 0x0800
# linux/if.h
IFF_PROMISC   = 0x100
# linux/sockios.h
SIOCGIFFLAGS  = 0x8913 # get the active flags
SIOCSIFFLAGS  = 0x8914 # set the active flags

###
# ifreq struct

class ifreq(ctypes.Structure):
    _fields_ = [("ifr_ifrn", ctypes.c_char * 16),
                ("ifr_flags", ctypes.c_short)]

# htons: converts 16-bit positive integers from host to network byte order
s = socket.socket(socket.PF_PACKET, socket.SOCK_RAW, socket.htons(ETH_P_ALL))

ifr = ifreq()
ifr.ifr_ifrn = b'eth0'
fcntl.ioctl(s, SIOCGIFFLAGS, ifr) # get the flags
ifr.ifr_flags |= IFF_PROMISC # add the promiscuous flag
fcntl.ioctl(s, SIOCSIFFLAGS, ifr) # update

pkg = s.recvfrom(BUFF_SIZE)
print(pkg)

ifr.ifr_flags ^= IFF_PROMISC # mask it off (remove)
fcntl.ioctl(s, SIOCSIFFLAGS, ifr) # update
