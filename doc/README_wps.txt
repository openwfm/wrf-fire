
Running the default case for wrffire with real landfire data.

!!! This data case only starts a fire somewhere in the mountains
!!! of Colorado on the date of August 28, 2005.  I make no claim 
!!! that it makes any sense physically, only that it runs on our 
!!! test computer, wf.cudenver.edu.

#in wrfv2_fire compile wrf em_real
./compile em_real

#in WPS compile wps
./compile wps

#link the atmospheric data for the target date 
#into the current directory
./link_grib.csh /home/jbeezley/wrfdata/Katrina/* 

#link atmospheric data descriptor into the current directory
ln -sf ungrib/Variable_Tables/Vtable.AWIP Vtable

#run geogrid to set up static geographical data (including landfire)
./geogrid.exe

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

#run wrf
./wrf.exe

*****************************************************************************

To set up a new case for a real data fire simulation, the procedure is similar
to the standard documentation for WPS.  First you must edit the namelist.wps
for the domain you wish to simulate.  (The namelist variables sr_x/sr_y
describe the subgrid scaling as in wrffire.)  Then you must download
atmospheric boundary conditions for the day you want, for instance from here:
http://dss.ucar.edu/datasets/ds083.2/data/

This data and variable table must be linked into the WPS directory as above.
If you use ucar/ncep data the variable table is Vtable.AWIP. 

Finally, run the wps programs geogrid, ungrib, and metgrid.  Then go to the
wrf real test directory, link in the metgrid output, and set up the namelist 
for the new domain.  (If the domain description differs from the wps namelist,
real.exe will not run successfully.)  Run real.exe and wrf.exe.

!!! geogrid gets its static geographical data from one of my directories,
!!! /home/jbeezley/wrfdata/geog 
!!! This is defined in the namelist, this directory currently only contains
!!! landfire data for the state of colorado, if your domain is outside this
!!! area, then nfuel_cat will contain zeros where no data is available.
!!! Contact me, jon.beezley.math@gmail.com if you want me to include data 
!!! for other regions.

