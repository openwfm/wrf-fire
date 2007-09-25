
To run first make sure you have set the NETCDF variable, this can be done by
sourcing the env.sh or env.csh script.  Then:

make -f testmakefile model_test

This will copy a file (fire_io_save.nc) to fire_io.nc and run the model.  This
is an input file that causes the model to crash.  To test the modified model
with the default initialization, do the following (in bash):

rm fire_io.nc
for ((i=1;i<=100;i++)) ; do 
./model_test_prog.x
done

This will run 100 10 s time steps with identical output to the jm branch.
