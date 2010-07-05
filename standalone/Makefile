# defines FC, FCFLAGS, FFREE
include make.inc
DEFS =
WRF = ../wrfv2_fire/phys
FPFLAGS= -I$(NETCDF)/include -I.
LDFLAGS = -L$(NETCDF)/lib -lnetcdf
FFLAGS=$(FPFLAGS) $(FCFLAGS) $(FFREE)


MAKEFILE=Makefile

%.o: $(WRF)/%.F 
	$(FC) $(FFLAGS) $(DEFS) -E $(WRF)/$*.F > $*.f90
	$(FC) -c $(FFLAGS) $*.f90

%.o: %.F 
	$(FC) $(FFLAGS) $(DEFS) -E $*.F > $*.f90
	$(FC) -c $(FFLAGS) $*.f90

SFIRE_OBJ = \
    module_fr_sfire_model.o \
    module_fr_sfire_core.o \
    module_fr_sfire_phys.o \
    module_fr_sfire_util.o  \
    wrf_fakes.o

MODEL_OBJ = model_test_main.o $(SFIRE_OBJ) wrf_netcdf.o
FUEL_INTERP_OBJ = fuel_interp_test_main.o $(SFIRE_OBJ) 

# targets

model: model_test.exe

fuel: fuel_interp_test.exe

model_test.exe: $(MAKEFILE) $(MODEL_OBJ) 
	$(FC) $(FCFLAGS) $(LDFLAGS) -o model_test_main.exe $(MODEL_OBJ)
 
fuel_interp_test.exe: $(MAKEFILE) $(FUEL_INTERP_OBJ)
	$(FC) $(FCFLAGS) $(LDFLAGS) -o fuel_interp_test.exe $(FUEL_INTERP_OBJ)
 
# for the order of generating modules


fuel_interp_test_main.o: $(MAKEFILE) \
    module_fr_sfire_core.o  \
    module_fr_sfire_util.o \
    wrf_fakes.o

module_fr_sfire_model.o: $(MAKEFILE) \
    module_fr_sfire_core.o  \
    module_fr_sfire_util.o 

module_fr_sfire_core.o: $(MAKEFILE) \
    module_fr_sfire_phys.o \
    module_fr_sfire_util.o 
      
module_fr_sfire_phys.o: $(MAKEFILE) \
    module_fr_sfire_util.o 

module_fr_sfire_util.o: $(MAKEFILE) \
    wrf_fakes.o

wrf_netcdf.o: $(MAKEFILE) \
    module_fr_sfire_util.o

clean:
	rm -f *.mod *.o *.f90 *.s *.exe
