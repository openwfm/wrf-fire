from netCDF4 import Dataset
import sys

fnamefrom=sys.argv[1]
fnameto=sys.argv[2]

ffrom=Dataset(fnamefrom,'r')
fto=Dataset(fnameto,'r+')

for n,v in ffrom.variables.iteritems():
    if fto.variables.has_key(n):
        fto.variables[n][:]=v[:]

fto.sync()
fto.close()
