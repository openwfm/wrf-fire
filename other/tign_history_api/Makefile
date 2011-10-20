
FC=gfortran

NETCDFLIB=$(shell nc-config --flibs 2> /dev/null || echo -L$(NETCDF)/lib -lnetcdf)
NETCDFINC=$(shell nc-config --fflags 2> /dev/null || echo -I$(NETCDF)/include)

all: tign_test.x

tign_history.o: tign_history.F90
	$(FC) -c $(NETCDFINC) $<

tign_test.x: tign_history.o tign_test.F90
	$(FC) -c tign_test.F90 -I.
	$(FC) -o $@ tign_test.o tign_history.o $(NETCDFLIB)

clean: 
	rm -f *.o *.x *.mod
