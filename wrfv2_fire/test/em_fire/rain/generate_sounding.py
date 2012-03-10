#!/usr/bin/env python

import sys

from netCDF4 import Dataset


fname=sys.argv[1]

f=Dataset(fname,'r')
nx=len(f.dimensions['west_east'])
ny=len(f.dimensions['south_north'])

i=nx/2
j=ny/2

hgt=(f.variables['PHB'][0,:,j,i] + f.variables['PH'][0,:,j,i])/9.8
hgt=(hgt[1:]+hgt[:-1])/2
t=f.variables['T'][0,:,j,i]+300
u=f.variables['U'][0,:,j,i]
v=f.variables['V'][0,:,j,i]
m=f.variables['QVAPOR'][0,:,j,i]*1000

s=open('input_sounding','w')
s.write('%10.2f\t%10.2f\t%10.2f\n' % (hgt[0],t[0],m[0]))
for k in xrange(len(hgt)):
    s.write('%10.2f\t%10.2f\t%10.2f\t%10.2f\t%10.2f\n' % (hgt[k],t[k],m[k],u[k],v[k]) )
s.close()
