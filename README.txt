WRF-Fire is a coupled atmosphere-wildland fire model combining WRF and
fire model code, released and distributed by NCAR as a part of WRF. 
This repository contains a development version of WRF-Fire with the latest
features and bug fixes, and additional utilities.
The code from this repository is submitted to WRF release from time to time.

For terms and conditions see the file license.txt

Documentation is in progress. Current drafts of the technical documentation and
user's guide can be found at

http://www.openwfm.org/wiki/WRF-Fire_documentation
http://www.openwfm.org/wiki/WRF-Fire_publications

We hope you find the code useful. Please let us know if we can help. But
do us a favor and before you ask a question (such as, what is git?) 
please google first. You may need to find a local guru for Linux questions.

If you find this code useful please acknowledge our work by citing the papers
[1] or [2]. [2] describes the code as of WRF release 3.3 planned in Spring 2011 and 
this repository as of early November 2010. The arXiv version [3] will be updated
to keep up with the current development version (the master branch in this repository).
[4] is just an extended abstract but it mentions few more advanced features not
described anywhere else at the moment.

The physical fire model is based on CAWFE [5] but not all features from [5] 
are implemented here yet. 

Thank you for using WRF-Fire!

References

[1] Jan Mandel, Jonathan D. Beezley, Janice L. Coen, Minjeong Kim, Data 
Assimilation for Wildland Fires: Ensemble Kalman filters in coupled 
atmosphere-surface models, IEEE Control Systems Magazine, 2009, 
29, Issue 3, June 2009, 47-65. http://dx.doi.org/10.1109/MCS.2009.932224
Preprint available from http://arxiv.org/abs/0712.3965

[2] Jan Mandel, Jonathan D. Beezley, and Adam K. Kochanski, "Coupled 
atmosphere-wildland fire modeling with WRF-Fire version 3.3", Geoscientific Model 
Development Discussions 4, 497-545, 2011. http://dx.doi.org/10.5194/gmdd-4-497-2011

[3] Jan Mandel, Jonathan D. Beezley, and Adam K. Kochanski, "Coupled 
atmosphere-wildland fire modeling with WRF-Fire. http://arxiv.org/abs/1102.1343

[4] Jan Mandel, Jonathan Beezley, and Adam Kochanski, "An overview of 
the coupled atmosphere-wildland fire model WRF-Fire",
91st American Meteorological Society Annual Meeting], Seattle, WA, January 25, 2011. 
http://ams.confex.com/ams/91Annual/webprogram/Manuscript/Paper185887/ams2011-fire.pdf 

[5] J. L. Coen, Simulation of the Big Elk Fire using using coupled 
atmosphere-fire modeling,” International Journal of Wildland Fire, vol. 14,
no. 1, pp. 49–59, 2005


--------------------------------------------------------------------

CURRENT ACTIVITY

For current activity and development trends please check out
http://ccm.ucdenver.edu/wiki/User:Jmandel/blog
http://www.openwfm.org/wiki/WRF-Fire_development_notes

SUPPORT

For support please subscribe to the wrf-fire mailing list at NCAR at
http://mailman.ucar.edu/mailman/listinfo/wrf-fire
or see http://www.openwfm.org/wiki/WRF-Fire_user_support 

NOTE: Answers to any questions may be copied or summarized to the wrf-fire list
and/or the wiki.


COMPILERS

Tested on linux/ifort/gcc, linux/pgi, linux/g95 (all x86-64), and mac/g95. 
Mac will not run optimized or real data, though. ifort for Linux can be 
downloaded from Intel free for non-commercial use. gfortran is not included 
because gfortran currently cannot compile wrf.


SETUP 

We strongly recommend using git not a tar file to download the code
so that you can get updates easily and also keep your changes. 
You will probably need to install git.  A current version (now 1.6) 
recommended. You may need to install git from sources.

Download NETCDF and install. The curent version is 4.0 Some hints: 
Set the environment variables CC FC and F90 to your compilers. 
Set the install location by something like 
./configure --prefix=/opt/netcdf 
(Of course, use your own location)
Set the environment variable to the top level netcdf directory like:
setenv NETCDF /opt/netcdf
(Of course, replace the path by your location of NETCDF above)

Local developers can source env.csh with setup for some local machines where
NETCDF is already installed.


DOWNLOAD SITES

The code can be downloaded from http://github.com/jbeezley/wrf-fire/ by

 	 git clone git://github.com/jbeezley/wrf-fire.git

You can see current activities, branches, and download the code also from

         http://repo.or.cz/git-browser/by-commit.html?r=wrffire.git

Developers with write access get the code and submit changes by 

         git clone ssh://user@math.ucdenver.edu/home/grads/jbeezley/wrf-fire.git

where "user" is your username at math.ucdenver.edu.


BRANCHES

Use the code from the master branch as it is the only stable one. Use other
branches only if instructed by the developers. We recommend to set up your own
branch for your changes and merge master into it when it is updated.
See doc/README_git.txt


BUILDING

 
cd wrfv2_fire
./configure # select an option for your computer architecture/compilers
	    # If you are trying to get this working on different
	    # compilers, try copying a section close to your case
            # in arch/configure_new.defaults to the end of that file
            # and modifying to suit your needs... good luck.
./compile em_fire >& compile.log

This will take a while.  Make sure that compile.log contains 
no errors.  ("grep Error compile.log" shouldn't return anything).


RUNNING

cd test/em_fire/small 
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

