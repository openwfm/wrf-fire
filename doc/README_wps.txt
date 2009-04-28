#!/bin/csh -f
#This is doc/README_wps.txt
#Running the default case for wrffire with real landfire data.
#You can run this file also as wps.csh from the top level wrf directory

#!!! This data case only starts a fire somewhere in the mountains
#!!! of Colorado on the date of August 28, 2005.  I make no claim 
#!!! that it makes any sense physically, only that it runs

#!!! run ./configure in the WPS and in the wrfv2_fire directories
#!!! this is interactive so it is not a part of this script

# get a copy of data: in the parent of the wrf directory
# note: this is about 11GB 
##(cd ..; rsync -arvzuP math.cudenver.edu:/home/faculty/jmandel/wrfdata .)
#!!! THE PREVIOUS LINE IS FOR LOCAL DEVELOPERS WITH ACCOUNT ON MATH ONLY
#!!! ALL OTHERS SEE "SET UP A NEW CASE" BELOW

#in wrfv2_fire compile wrf em_real
(cd wrfv2_fire; ./compile em_real)

# next few commands run in WPS
cd WPS

#in WPS compile wps
./compile wps

#in WPS link the atmospheric data
./link_grib.csh ../../wrfdata/Katrina/*

#in WPS link the atmospheric data descriptor
ln -sf ungrib/Variable_Tables/Vtable.AWIP Vtable

#in WPS run geogrid to set up static geographical data, including landfire
#This command will run a series of python scripts that grab data specific
#to your domain and convert it to geogrid format.  If everything goes well,
#it will run geogrid.exe with the new data.
#!!! REQUIRES a number of external utilities:
#!!!   Python 2.5+ (www.python.org)
#!!!   GDAL library and python bindings (www.gdal.org)
#!!!     (See the binary distributions in the downloads section.)
#!!!   twill (http://twill.idyll.org)
#!!!     (Install with python's easy_install or simply extract the source somewhere
#!!!      and add that path to PYTHONPATH.)
#
#If you can't satisfy these dependencies, you must generate fuel category and 
#highres topological data on your own.  The datasets are simply too big to distribute
#in one package like the rest of geogrid.
./geogrid_wrapper.sh

#run ungrid to set up atmospheric data
./ungrib.exe

#run metgrid to interpolate fields to the model grid
./metgrid.exe

#go to wrf run directory
cd ../wrfv2_fire/test/em_real

#link metgrid output here
ln -sf ../../../WPS/met_em.d01.2005-08-28_1* .

#run wrf preprocessor
./real.exe

#run wrf.exe in wrf/wrfv2_fire/test/em_real
#./wrf.exe

#*****************************************************************************
#
# SET UP A NEW CASE
#
#To set up a new case for a real data fire simulation, the procedure is similar
#to the standard documentation for WPS.  First you must edit the namelist.wps
#for the domain you wish to simulate.  (The namelist variables sr_x/sr_y
#describe the subgrid scaling as in wrffire.)  Then you must download
#atmospheric boundary conditions for the day you want, for instance from here:
#http://dss.ucar.edu/datasets/ds083.2/data/
#
#This data and variable table must be linked into the WPS directory as above.
#If you use ucar/ncep data the variable table is Vtable.AWIP. 
#
#Finally, run the wps programs geogrid, ungrib, and metgrid.  Then go to the
#wrf real test directory, link in the metgrid output, and set up the namelist 
#for the new domain.  (If the domain description differs from the wps namelist,
#real.exe will not run successfully.)  Run real.exe and wrf.exe.
#
