Index of documentation in doc subdirectory

README.txt                  this file, how to compile and run WRF/SFIRE on ideal data
README_git.txt              how to use the versioning system
README_mac.txt              how to run on a Mac
README_matlab_netcdf.txt    how to read WRF input and output directly Matlab
README_vis.txt              matlab visualization using files written every timestep
README_visualization.txt    convert WRF input and output to Matlab readable format
README_wps.txt              how use real data including fuel from Landfire


*** All is set up to run on wf.cudenver.edu ONLY ***
*** See the bottom of this file how to set up another machine ***


BRANCHES

The repository contains these main branches:

* master : stable branch 
* jbeezley  
* jc 
* jm: current working branch
* jm-base: early snapshot that gives the same output as Ned had

Only Jon can commit to master branch in the repository.
Each branch has owner and one the owner can commit to that branch in the
repository.
If you want to have more than one branch have Jon set up a branch (say, jm2)
and then use it as a kind of directory, as in notation, as jm2/newtest

RUNNING

Make sure you source either env.sh (for sh shells) 
or env.csh (for csh shells) to set up environment
variables for building. Then,

 
cd wrfv2_fire
./configure # select an option for your computer,
            # the configuration options present have all been tested
	    # and should work correctly for the intended computer
	    # If you are trying to get this working on a different
	    # computer, try copying wrfv2_fire/arch/configure.defaults_orig
	    # to configure.defaults, this file contains all standard
	    # configuration options, but are untested... good luck.
./compile em_fire >& compile.log

This will take a while.  Make sure that compile.log contains 
no errors.  ("grep Error compile.log" shouldn't return anything).
Or, just run

./compile_fire

and then check compile.log only if you see any output.

Finally, run the code with:

cd test/em_fire 
./run_me_first.csh (needs to be done only once)
./ideal.exe >& ideal.log (this creates file wrfinput_d01, needs to be done only
when input_sounding changes)
./wrf.exe

Or, just run the debugger, it is set up to load the program automatically and set 
a breakpoint at exit, esp. error exit:

cd test/em_fire 
idb -dbx

This runs an "idealized" problem. For RUNNING ON REAL DATA see doc/README_wps.txt


STANDALONE FIRE MODEL TEST

with ifort:

cd phys
./ifmake clean
./ifmake model_test

Without the clean between different kind of builds nothing will work.
There is g95make for g95.
With other compilers, try someting like make -f testmakefile FC=your fortran compiler
See wrfv2_fire/phys/README_jm.txt for more info.




SETUP ON A NEW MACHINE

Tested on linux/ifort, linux/pgi, and mac/g95. Mac will not run optimized or real data, though.
Ask Jon if you need more architectures to be added to the menu that shows in ./configure.

You need to install NETCDF and modify the environment variables accordingly.
You can make your own copy of env.csh (under a different name)
to set up the environment but please do not commit changed env.csh

To install NETCDF: Donload from the web and install per the istructions therein.
The current and tested version is 4.0. Use the compilers you will use for wrf.

Set environment variable to the top level netcdf directory like:
setenv NETCDF /opt/wrf-libs/netcdf
(of course, replace the path by the location where you put netcdf)


