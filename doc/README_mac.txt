From Jon:

The command to get wrf is

git clone math.cudenver.edu:/home/grads/jbeezley/wrf.git

This of coarse requires git and an account on math.cudenver.edu.
There are mac binaries available.

The main directory contains README.txt, which contains relevant
information for compiling and running.  The only thing in there that
is not relevant is sourcing the env.sh or env.csh file.  This will not
work on your computer, so you will have to do the following:

You will have to compile and install netcdf on your computer.  I don't
recall having any problems with this.

F90=g95 FC=g95 F77=g95 ./configure --prefix=<prefix> && make && make install

should do the trick.  Then just set the environment variable: NETCDF
to the prefix where you installed netcdf.

setenv NETCDF <prefix>

for cshell or

export NETCDF=<prefix>

for bash.
