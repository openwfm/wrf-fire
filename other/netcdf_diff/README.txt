
Jonathan Beezley
2-15-2008

netcdf_diff.x is a simple utility meant to compare two netcdf files produced
by different versions of the same code.  It will output statistics like mean
absolute error, mean square error, and maximum error between two input files.
Because the program is designed to detect errors introduced by simple code
changes, it expects that the meta data in the netcdf files to be the same
between the files.  If the meta data differs, the program will issue a warning
and try to print out the differences anyway, in most cases.

To compile, make sure that the NETCDF environment variable is set.  If you are
using ifort, and it is in your PATH then it is sufficient to type:

make

otherwise you must specify the compiler with the standard environment
variables, F90 and FFLAGS.

Running the program is as simple as:

./netcdf_diff.x file1 file2

where file1 and file2 are the two files you wish to compare.

