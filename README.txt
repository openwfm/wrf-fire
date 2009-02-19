This is the coupled WRF-Fire model, combining WRF and the SFIRE codes.

For terms and conditions see the file license.txt

If you find this code useful please acknowledge our work by citing the paper
[1].  Yes this is a strange reference for an atmospheric scientist but it  
is the only one we have at the moment. Please see the sections
"Fireline propagation model", "Coupling ﬁre and weather models", and
"Level-set-based wildland fire model".  The fire model is based on [2] 
but not all features from [2] are implemented here yet.

We hope you find the code useful. Please let us know if we can help. But
do us a favor and before you ask a question (such as, what is git?) 
please google first. You may need to find a local guru for Linux questions.
Thank you!

Janice Coen - physics
Jan Mandel - the fire code
Jonathan Beezley - WRF infrastructure and WPS

[1] Jan Mandel, Jonathan D. Beezley, Janice L. Coen, Minjeong Kim, Data 
Assimilation for Wildland Fires: Ensemble Kalman filters in coupled 
atmosphere-surface models, IEEE Control Systems Magazine, 2009, to appear. 
Preprint available from http://arxiv.org/abs/0712.3965

[2] J. L. Coen, “Simulation of the Big Elk Fire using using coupled 
atmosphere-fire modeling,” International Journal of Wildland Fire, vol. 14,
no. 1, pp. 49–59, 2005

--------------------------------------------------------------------

SUPPORT

For support please subscribe to the wrf-fire mailing list at NCAR at
http://mailman.ucar.edu/mailman/listinfo/wrf-fire

You can also ask directly Jan.Mandel@ucar.edu

Answers to any questions may be copied or summarized to the wrf-fire list.


HOW TO GET THIS CODE

The code can be downloaded from http://github.com/jbeezley/wrf-fire/ by

 	 git clone git://github.com/jbeezley/wrf-fire.git

We strongly recommend using git not a tar file so that you can get updates
easily and also keep your changes. You may need to install git first.

Developers get the code and submit changes by 

         git clone ssh://math.cudenver.edu/home/grads/jbeezley/wrf.git


BRANCHES

Use the code from the master branch as it is the only stable one. Use other
branches only if instructed by the developers. We recommend to set up your own
branch for your changes and merge master into it when it is updated.
See doc/README_git.txt


SETUP ON A NEW MACHINE

Tested on linux/ifort, linux/pgi, and mac/g95. Mac will not run optimized 
or real data, though. ifort for Linux can be downloaded from Intel free for 
non-commercial use.

Download and install NETCDF first. 
The current and tested version is 4.0. Use the compilers you will use for wrf.

Set environment variable to the top level netcdf directory like:
setenv NETCDF /opt/wrf-libs/netcdf
(of course, replace the path by the location of NETCDF)

You also need to set the environment variables FC and F90 to your Fortran
compiler.

Developers can source env.csh with setup for some local machines. 


BUILDING

 
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


RUNNING

cd test/em_fire 
./run_me_first.csh (needs to be done only once)
./ideal.exe >& ideal.log (this creates file wrfinput_d01, needs to be done only
when input_sounding changes)
./wrf.exe

This runs an "idealized" problem. 
For RUNNING ON REAL DATA see doc/README_wps.txt


STANDALONE FIRE MODEL TEST

With ifort:

cd phys
./ifmake clean
./ifmake model_test

Without the clean between different kind of builds nothing will work.
There is g95make for g95.
With other compilers, try someting like 

    make -f testmakefile FC=your fortran compiler

See wrfv2_fire/phys/README_jm.txt for more info.

NOTE: maintaining the standalone model is low priority and it may not work.


ADDITIONAL DOCUMENTATION IN THE doc SUBDIRECTORY


README.txt                  this file, how to compile and run WRF/SFIRE on ideal data
README_git.txt              how to use the versioning system
README_mac.txt              how to run on a Mac
README_matlab_netcdf.txt    how to read WRF input and output directly in Matlab
README_vis.txt              matlab visualization using files written every timestep
README_visualization.txt    convert WRF input and output to Matlab
README_wps.txt              how use real data including fuel from Landfire

