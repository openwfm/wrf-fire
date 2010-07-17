# defines FC, FCFLAGS, FFREE
include make.inc
DEFS =
WRF = ../wrfv2_fire/phys
FPFLAGS= -I$(NETCDF)/include -I.
LDFLAGS = -L$(NETCDF)/lib -lnetcdf -g -O0 -C
FFLAGS=$(FPFLAGS) $(FCFLAGS) $(FFREE) -g -O0 -C


MAKEFILE=Makefile

%.o: $(WRF)/%.F 
	$(FC) $(FFLAGS) $(DEFS) -E $(WRF)/$*.F > $*.f90
	$(FC) -c $(FFLAGS) $*.f90

%.o: %.F 
	$(FC) $(FFLAGS) $(DEFS) -E $*.F > $*.f90
	$(FC) -c $(FFLAGS) $*.f90

MODEL_OBJ = \
    wrf_netcdf.o \
    wrf_namelist.o \
    module_fr_sfire_model.o \
    module_fr_sfire_core.o \
    module_fr_sfire_phys.o \
    module_fr_sfire_util.o  \
    wrf_fakes.o

FUEL_INTERP_OBJ = \
    fuel_interp_test_main.o \
    module_fr_sfire_core.o \
    module_fr_sfire_phys.o \
    module_fr_sfire_util.o  \
    wrf_fakes.o

# targets

.PHONY: all


all: model_test_main.exe model_test_init.exe

model_test_main.exe: $(MAKEFILE) model_test_main.o $(MODEL_OBJ)
	$(FC) $(FCFLAGS) $(LDFLAGS) -o model_test_main.exe model_test_main.o $(MODEL_OBJ)

model_test_init.exe: $(MAKEFILE) model_test_init.o $(MODEL_OBJ)
	$(FC) $(FCFLAGS) $(LDFLAGS) -o model_test_init.exe model_test_init.o $(MODEL_OBJ)

fuel: fuel_interp_test.exe
 
fuel_interp_test.exe: $(MAKEFILE) $(FUEL_INTERP_OBJ)
	$(FC) $(FCFLAGS) -o fuel_interp_test.exe $(FUEL_INTERP_OBJ)
 
# for the order of generating modules


fuel_interp_test_main.o: $(MAKEFILE) \
    module_fr_sfire_core.o  \
    module_fr_sfire_util.o \
    wrf_fakes.o

model_test_init.o: $(MAKEFILE) \
   wrf_namelist.o \
   wrf_netcdf.o \
   module_fr_sfire_util.o

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

wrf_namelist.inc: $(MAKEFILE) \
    wrf_netcdf.o \
    module_fr_sfire_util.o

clean:
	rm -f *.mod *.o *.f90 *.s *.exe
