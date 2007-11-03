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

Only Jon should commit to master branch.

RUNNING

Make sure you source either env.sh (for sh shells) 
or env.csh (for csh shells) to set up environment
variables for building. Then,

cd wrfv2_fire
./configure # select option #5 for building
./compile em_fire >& compile.log

This will take a while.  Make sure that compile.log contains 
no errors.  ("grep Error compile.log" shouldn't return anything).
Or, just run

./compile_fire

Finally, run the code with:

cd test/em_fire 
./run_me_first.csh
./wrf.exe

Or, just run the debugger, it is set up to load the program automatically and set 
a breakpoint at exit, esp. error exit:

cd test/em_fire 
idb

STANDALONE FIRE MODEL TEST

cd phys
./ifmake clean
./ifmake model_test
./ifmake prop_test
./ifmake clean

Without the clean betweed wrf built and standalone build, neither will work.
