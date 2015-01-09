# convert geotiff file to matlab file & print basic info on screen too
# usage: python geotiff2mat.py filename.tif
# limitation: for now, one band only

# tested on Python 3.4

# arguments
import sys

file=sys.argv[1]
infile=file
outfile = file + '.mat'

# read the geotiff file
# see http://geoinformaticstutorial.blogspot.com/2012/09/reading-raster-data-with-python-and-gdal.html

import gdal
from gdalconst import *
print('Reading GeoTiff file',infile)
dataset = gdal.Open(infile, GA_ReadOnly)
cols = dataset.RasterXSize
rows = dataset.RasterYSize
bands = dataset.RasterCount
print('rows                ',rows)
print('columns             ',cols)
print('bands               ',bands)
geotransform = dataset.GetGeoTransform()
print('top left X          ',geotransform[0])
print('W-E pixel resolution',geotransform[1])
print('rotation, 0=North up',geotransform[2])
print('top left Y          ',geotransform[3])
print('rotation, 0=North up',geotransform[4])
print('N-S pixel resolution',geotransform[5])
band = dataset.GetRasterBand(1)
data = band.ReadAsArray(0, 0, cols, rows)

# write matlab file
# using http://docs.scipy.org/doc/scipy-0.14.0/reference/tutorial/io.html
import scipy.io as sio
print('writing Matlab file ',outfile)
sio.savemat(outfile,{'data' : data,'geotransform' : geotransform})

exit()

