include make.inc
FC = gfortran 
DEFS =
WRF = ../wrfv2_fire/phys

MAKEFILE=Makefile

%.o: $(WRF)/%.F 
	$(FC) $(FCLAGS) $(FFREE) $(DEFS) -E $(WRF)/$*.F > $*.f90
	$(FC) -c $(FFLAGS) $(FFREE) $*.f90

%.o: %.F 
	$(FC) $(FCLAGS) $(FFREE) $(DEFS) -E $*.F > $*.f90
	$(FC) -c $(FFLAGS) $(FFREE) $*.f90

MODEL_OBJ = \
    model_test_main.o \
    wrf_netcdf.o \
    module_fr_sfire_model.o \
    module_fr_sfire_core.o \
    module_fr_sfire_phys.o \
    module_fr_sfire_util.o  \
    wrf_fakes.o

model_test_main.exe: $(MODEL_OBJ)
	$(FC) $(LDFLAGS) -o model_test_main.exe $(MODEL_OBJ)
 
# for the order of generating modules

model_test_main.o: \
    module_fr_sfire_model.o  \
    module_fr_sfire_util.o \
    wrf_netcdf.o

module_fr_sfire_model.o: \
    module_fr_sfire_core.o  \
    module_fr_sfire_util.o 

module_fr_sfire_core.o: \
    module_fr_sfire_phys.o \
    module_fr_sfire_util.o 
      
module_fr_sfire_phys.o: \
    module_fr_sfire_util.o 

module_fr_sfire_util.o: \
    wrf_fakes.o

wrf_netcdf.o: \
    module_fr_sfire_util.o

clean:
	rm -f *.mod *.o *.f90 *.s *.exe
