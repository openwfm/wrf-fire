# fefines FC, FCFLAGS, FFREE
include make.inc
DEFS =
WRF = ../wrfv2_fire/phys
FPFLAGS= -I$(NETCDF)/include -I.
LDFLAGS = -L$(NETCDF)/lib -lnetcdf
FFLAGS=$(FPFLAGS) $(FCFLAGS) $(FFREE)


MAKEFILE=Makefile

%.o: $(WRF)/%.F 
	$(FP) $(FFLAGS) $(DEFS) -E $(WRF)/$*.F > $*.f90
	$(FC) -c $(FFLAGS) $*.f90

%.o: %.F 
	$(FP) $(FFLAGS) $(DEFS) -E $*.F > $*.f90
	$(FC) -c $(FFLAGS) $*.f90

MODEL_MAIN_OBJ = \
    module_domain.o \
    wrf_netcdf.o \
    module_configure.o \
    module_fr_sfire_driver.o \
    module_fr_sfire_model.o \
    module_fr_sfire_core.o \
    module_fr_sfire_phys.o \
    module_fr_sfire_util.o  \
    wrf_fakes.o

MODEL_INIT_OBJ = \
    wrf_netcdf.o \
    module_configure.o \
    module_fr_sfire_util.o  \
    wrf_fakes.o

NAMELIST= \
    namelist_statements.inc \
    namelist_defaults.inc \
    namelist_defines.inc \
    namelist_defines2.inc \
    config_assigns.inc

#.PHONY: all


default: model_test_main.exe 

all: model_test_main.exe model_test_init.exe

model_test_main.exe: $(MAKEFILE) model_test_main.o $(MODEL_MAIN_OBJ)
	$(FC) $(FCFLAGS) -o model_test_main.exe model_test_main.o $(MODEL_MAIN_OBJ) $(LDFLAGS)

model_test_init.exe: $(MAKEFILE) model_test_init.o $(MODEL_INIT_OBJ)
	$(FC) $(FCFLAGS) $(LDFLAGS) -o model_test_init.exe model_test_init.o $(MODEL_INIT_OBJ) $(LDFLAGS)
 
# for the order of generating modules

model_test_main.o: $(MAKEFILE) \
    module_domain.o \
    module_configure.o \
    module_fr_sfire_driver.o \
    wrf_fakes.o \
    wrf_netcdf.o

module_fr_sfire_driver.o: $(MAKEFILE) \
    module_domain.o \
    module_configure.o \
    module_fr_sfire_model.o  \
    module_fr_sfire_phys.o \
    module_fr_sfire_util.o \

model_test_init.o: $(MAKEFILE) \
    module_configure.o \
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
    sfire_id.inc \
    wrf_fakes.o

wrf_netcdf.o: $(MAKEFILE) \
    module_fr_sfire_util.o

module_configure.o: $(MAKEFILE) $(NAMELIST) \
    wrf_netcdf.o \
    module_fr_sfire_util.o

sfire_id.inc: always
	chmod +x commit_hash
	./commit_hash > sfire_id.inc
	cat sfire_id.inc

always:

clean:
	rm -f *.mod *.o *.f90 *.s *.exe
