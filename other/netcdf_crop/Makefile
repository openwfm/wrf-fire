FC=ifort
FCFLAGS= -free -I$(NETCDF)/include -g
LFLAGS=-L$(NETCDF)/lib -lnetcdf 

netcdf_crop.x: Makefile netcdf_crop.F
	$(FC) $(FCFLAGS) -o netcdf_crop.x netcdf_crop.F $(LFLAGS)
