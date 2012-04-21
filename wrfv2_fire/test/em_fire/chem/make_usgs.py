#!/usr/bin/env python

import sys,os

try:
   from netCDF4 import Dataset
except ImportError:
    print 'Requires netcdf4-python (http://code.google.com/p/netcdf4-python/)'
    sys.exit(1)


if not os.path.exists('wrfinput_d01'):
    print 'First run ./ideal.exe and then this script.'
    sys.exit(1)

f=Dataset('wrfinput_d01','r+')
f.MMINLU='USGS'
f.close()
