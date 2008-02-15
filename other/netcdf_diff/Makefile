
F90?=ifort
FFLAGS?=-O2
FFLAGS:=${FFLAGS} -I${NETCDF}/include -L${NETCDF}/lib -lnetcdf
netcdf_diff.x: netcdf_diff.F90
	${F90}  netcdf_diff.F90 -o netcdf_diff.x ${FFLAGS}

clean:
	- rm -fr *.o *.x *.mod
