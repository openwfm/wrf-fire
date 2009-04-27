#!/bin/bash

##############################################################################
#
# geogrid_wrapper.sh
#
# A simple wrapper script for geogrid.exe that attempts to fetch data from
# a usgs server if it fails from a halt_on_missing error.  This script relies
# on a number of python scripts assumed to be in '../other/convert_to_geogrid'.
# It needs python 2.5+ as well as two external python modules:
#
#   twill (http://twill.idyll.org/)
#   gdal (http://www.gdal.org/)
#
# twill can simply be extracted into a directory inside your PYTHONPATH, but
# gdal requires a number of C libraries.  I recommend finding a binary 
# distribution from 'http://trac.osgeo.org/gdal/wiki/DownloadingGdalBinaries' 
# rather than compiling your own.
#
# Author: Jonathan Beezley (jon.beezley.math@gmail.com)
# Date: April 26, 2009
#
##############################################################################

pypath="../other/convert_to_geogrid"
PYTHONPATH="${PYTHONPATH}:${pypath}" python ${pypath}/geogrid_wrapper.py ${@}
