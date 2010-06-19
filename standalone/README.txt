This directory contains SFIRE (the fire component of wrf-fire) test driver.
It links the fire model from the wrfv2_fire/phys/module_fr_sfire_*.F file with the 
files in this directory:

model_test_main.F	the main program that calls the model
wrf_fakes.F             stubs that perform various wrf functions

To build the standalone driver, create your own make.inc, or link an existing one, and type make. 
This will create file model_test_main.exe, then just execute this file.

All I can say at the moment is that the standalone model runs and does something.
Some sensible input and output comes later.

Jan Mandel
June 18, 2010
