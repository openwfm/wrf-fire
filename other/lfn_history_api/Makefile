
FC=gfortran

all: lfn_test.x

lfn_history.o: lfn_history.F90
	$(FC) -c -I$(NETCDF)/include $<

lfn_test.x: lfn_history.o lfn_test.F90
	$(FC) -c lfn_test.F90 -I.
	$(FC) -o $@ lfn_test.o lfn_history.o -L$(NETCDF)/lib -lnetcdf
