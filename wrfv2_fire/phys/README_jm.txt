This directory contains the fire model in development. It is possible to test
the model independently of wrf.

To test the abstract core (mathematical model):
./ifmake clean core_test

To test the complete fire model (with physics):
./ifmake clean model_test

To test the complete fire model through atm inteface: 
./ifmake clean atm_test

When repeating the build on the same test, you can omit the "clean".

ifmake is for the Intel compiler. There is also g95make for g95. These use
testmakefile not Makefile. Makefile is for wrf only.

Jan Mandel
September 2007

