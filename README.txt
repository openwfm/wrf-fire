See documentation in 'doc' directory for more information.
See file doc/README_git.txt on version control

*** Currently all is set up to run on wf.cudenver.edu ONLY ***


BRANCHES

The repository contains these main branches:

* master : stable branch 
* jbeezley  
* jc 
* jm: current working branch
* jm-base: early snapshot that gives the same output as Ned had

Only Jon should commit to master branch in the repository.
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

Finally, run the code with:

cd test/em_fire 
./run_me_first.csh (needs to be done only once)
./ideal.exe >& ideal.log (this creates file wrfinput_d01, needs to be done only
when input_sounding changes)
./wrf.exe

Or, just run the debugger, it is set up to load the program automatically and set 
a breakpoint at exit, esp. error exit:

cd test/em_fire 
idb

STANDALONE FIRE MODEL TEST

cd phys
./ifmake clean
./ifmake model_test
./ifmake clean
./ifmake prop_test
./ifmake clean

Without the clean between different kind of builds nothing will work.

*NOTE*
On walnut, the ifmake script will not work, but you can use the makefile, 
'testmakefile.walnut' like this:

make -f testmakefile.walnut clean model_test

this will run the standalone model test.

SETUP ON A NEW MACHINE

You need to install the NETCDF and LAPACK libraries, make
your copy of env.csh, and modify the environment variables accordingly.
