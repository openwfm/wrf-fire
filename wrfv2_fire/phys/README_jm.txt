This is wrf/wrfv2_fire/README_jm.txt

This directory contains the fire model in development. The model can
be also run independently of wrf.

Overall structure:

layer                   description         tester            status

WRF                     atmospheric model   compile em_fire
module_fr_sfire_atm     atm-fire coupling   ifmake atm_test   compiles
module_fr_sfire_model   fire model w/phys   ifmake model_test runs
module_fr_sfire_core    complete math model ifmake core_test  runs
module_fr_sfire_prop    math propagation    ifmake prop_test  runs
module_fr_sfire_burn    fuel consumption    used by core_test
module_fr_sfire_speed   spread formulas     used by model_test
module_fr_sfire_util    utilities           ifmake util_test
module_fr_sfire_fuel    fuel tables         used by model_test
module_fr_sfire_params  arrays passed to speed

Each module is in its own .F file. All files are in
wrf/wrfv2_fire/phys.

ifmake is a script for the Intel compiler. There is also g95make for
g95.

These use testmakefile not Makefile. Makefile is for wrf only.
core_test and prop_test use their own simplified spread function and
so they do notrefer to the physics in any way. model_test uses the
actual Rothermel's formulas from module_fr_sfire_speed. Visualization
in testers is by Matlab. Instructions are provided when running the
tester.

Dependencies:  --> means call/use

WRF --> atm --> model --> core --> prop --> speed
                 |         |                |   |
                 |         ------> burn     |   |
                 |                          |   |
                 |                          V   |
                  -----------------------> fuel |
                 |                              V
                 |----------------------- > params

Everybody uses util. Other than that, use of modules is permitted
only along the arrows above. For example, atm may call only
subroutines from model, model may not call a subroutine from prop,
and prop may not use an array declared in params. This is to make
possible the development and testing of the math algorithms
independently of the physics.

Only util may call WRF procedures directly, everybody else must call
WRF wrappers provided in util. This is to keep the fire code
independent of WRF. Fake versions of some WRF procedures are linked
in the testers.

The code currently violates some WRF conventions, esp. uses heap
memory, and will not run in parallel. This will be fixed later, after
the whole coupled atmosphere-fire model runs correctly.

How to call model from atm should be clear from model_test_main.F.

The fire code is added to the WRF build via Makefile in this
directory, but it is not called yet. The code currently in
module_fr_sfire_atm is rather fragmental and partly copied from Ned's
code.

It is planned to replace prop by a better algorithm in future. What
is in there works but may not be accurate and is not optimized. For
example, the current version seems to add a bit of spread rate even
in examples with wind only and zero spread rate.

Jan Mandel
September 2007
