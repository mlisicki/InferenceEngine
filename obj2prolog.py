#!/usr/bin/env python2

import sys

f = open(sys.argv[1], 'r')

for line in f:
    if line[0]=='v':
        sl = line.split()
        print 'vertex({0},{1},{2})'.format(sl[1],sl[2],sl[3])

