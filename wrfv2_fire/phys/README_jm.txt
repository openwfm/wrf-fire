This is wrf/wrfv2_fire/README_jm.txt

This directory contains the fire model in development. The model can
be also run independently of wrf.

Overall structure:

layer                   description         tester            status

WRF                     atmospheric model   compile em_fire
module_fr_sfire_driver  atm-fire coupling   ifmake driver_test compiles
module_fr_sfire_model   fire model w/phys   ifmake model_test runs
module_fr_sfire_core    complete math model ifmake core_test  runs
module_fr_sfire_phys    all physics         used by model and core
module_fr_sfire_util    utilities           ifmake util_test

Each module is in its own .F file. All files are in
wrf/wrfv2_fire/phys.

ifmake is a script for the Intel compiler. There is also g95make for
g95.

These use testmakefile not Makefile. Makefile is for wrf only.
core_test and prop_test use their own simplified spread function and
so they do not refer to the physics in any way. model_test uses the
actual Rothermel's formulas from module_fr_sfire_phys. Visualization
in testers is by Matlab. Instructions are provided when running the
tester.

Software dependencies:

                  ----------------------> util --> WRF utilities
                 |          |         |
WRF --> driver --> model --> core --->   |
         |       |          |       phys
          ------------------------>

         WRF MEDIATION LAYER <--|-->  WRF PHYSICS LAYER

Everybody uses util. Other than that, use of modules is permitted
only along the arrows above. For example, atm may not call directly
subroutines from core. This is to make possible the development and
testing of the math algorithms independently of the physics.

Only util may call WRF procedures directly, everybody else must call
WRF wrappers provided in util. This is to keep the fire code
independent of WRF. Fake versions of the WRF procedures used in util 
are in wrf_fakes.F.

Arrays that need to be passed to function fire_ros in
module module_fr_sfire_phys are added to the argument lists of all
subroutines in the calling chain.

The code may not maintain any non-constant variables or arrays, and
may not maintain any arrays with variable bounds. All state information must be passed through the argument list of the driver. This is a WRF coding restriction.

How to call the model should be clear from model_test_main.F.

The fire code is added to the WRF build via Makefile in this
directory, but it is not called yet. The code currently in
module_fr_sfire_atm is rather fragmental and partly copied from Ned's
code.

It is planned to replace prop by a better algorithm in future. What
is in there works but may not be accurate and is not optimized. For
example, the current version seems to add a bit of spread rate even
in examples with wind only and zero spread rate.

Jan Mandel September 2007 - October 2008
