This directory contains the fire model in development. 

To test the abstract core:
./ifmake core_test

To test the complete fire model: 
./ifmake model_test

To test the complete fire model with atm inteface (not even compiling yet): 
./ifmake atm_test

If something strange happens try

./ifmake clean

ifmake is for the Intel compiler. There is also g95make. These use
Makefile_cvs not Makefile. Makefile is for wrf only.

See ../../README_cvs.txt for more.

Jan Mandel September 13 2007
