#!/usr/bin/python
import sys

a=0
b=0
c=0
for line in sys.stdin:
	bits = line.split("\t")
	a = a + int(bits[0])
	b = b + int(bits[1])
	c = c + int(bits[2])

if a:
	ratio = float(b) / float(a)
else:
	ratio = float(0)

print "%s\t%d\t%d\t%d\t%f" % (sys.argv[1], a, b, c, ratio)
