For Jan's fire visualization via .txt files see wrf/other/Matlab/vis
For Jan's fire visualization via wrfrst* files see wrf/other/Matlab/netcdf
--------------------------------------------------------------------


WRF visualization guide
Jonathan Beezley 
06-09-2007
=======================

1. WRF I/O

The io format used by WRF is netcdf.  The netcdf library is installed 
at $NETCDF/lib  (NETCDF is defined by wrf/env.csh and wrf/env.sh).  
When you wish to run WRF, the following files must be present:
  wrfbdy_dxx
  wrfinput_dxx
They specify the initial and boundary conditions for the run for each 
domain, dxx.  

WRF produces output files of the form:
  wrfout_dxx_yyyy-mm-dd_HH:MM:SS
  wrfrst_dxx_yyyy-mm-dd_HH:MM:SS
where xx is the domain number, and the rest is the date/time of the output
in the simulation time.  You can control when WRF will output these files
by editing namelist.input in the run directory, for example
  history_interval = 10,60,60,
  restart_interval = 180,
will output a history file every 10 minutes for domain 1 and every 60 minutes
for domains 2 and 3.  WRF will stop running after 3 hours (in simulation time)
and output a restart file, which you can use to restart the run where it left 
off.  Necessarily, the restart file will contain more variables because it 
requires all (non-derivable) run time variables.  What gets put into these
files is configureable by the Registry, but this is outside the scope of this
document.

2. NETCDF files

NETCDF creates compressed, binary data files which must be interpreted through
functions in libnetcdf.a.  The source is distributed with a simple command line
program called ncdump, which allows you to quickly get information about the
contents of the data files.  The standard ncdump executable is located at 
  $NETCDF/bin/ncdump
Execute the binary with no arguments to get usage information.  Two useful flags
are:
  -h : outputs header information only (variable descriptions, sizes, etc)
  -v var : outputs only the variable named var
NOTE: Executing with a flag, but no file (like ncdump -h) name will cause a segfault.
      This is how the standard release is, don't blame me.

In addition to the standard ncdump, I have written a modified version that accepts
an additional flag (-w).  The modified version of the source is in 
  wrf/other/netcdf_write_matrix
You can compile it on your own using the instructions in 
  wrf/other/netcdf_write_matrix/README_WRITE_MATRIX
or you can just use ~jbeezley/bin/ncdump
The purpose of this version is to output files formatted as write_matrix from
libutil1_jan.a so that our matlab scripts will read them.  You should be able to use
any other flag given by the usage statement in addition to -w.  
EXAMPLE:
  ncdump -w file : output all variables in file to separate files VAR.txt, where
                   VAR is the variable name.
  ncdump -v VAR -w file : output only VAR.txt

LIMITATION:
  ncdump will not output any variables with metadata indicated that it is > 3 diminnsional.

3. NCARG and NCL

I have had limited luck using these for several reasons, mostly because NCL 
is closed source and the scripts are poorly written (pgf77 hard coded, etc.)  
I'll update here if I figure out some repeatable instructions, until then
GOOD LUCK.

4.  MEXCDF

Matlab scripts for visualization and importation of netCDF data.  These
scripts seem to be very useful, but they don't work with Matlab versions >6.
Perhaps there is a simple fix somewhere, but I haven't been able to get it to
work.

5.  OTHERS

??
