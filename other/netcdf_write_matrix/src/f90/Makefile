include ../macros.make

# Override the definition for "ncdump" in "../macros.make".
NCDUMP          = ../ncdump/ncdump

LIBRARY         = ../libsrc/libnetcdf.a
LIB_OBJS        = netcdf.o typeSizes.o
GARBAGE         = netcdf_test *.mod *.d example.nc example.cdl \
		  eightByteIntExample.nc
MANUAL          = netcdf.3f90
PACKING_LIST    = \
		Makefile \
		NOTES \
		f90aux.m4 \
		example_good.cdl \
		gen.m4 \
		netcdf.3f90 \
		netcdf.f90 \
		netcdf_attributes.f90 \
		netcdf_constants.f90 \
		netcdf_dims.f90 \
		netcdf_expanded.f90 \
		netcdf_externals.f90 \
		netcdf_file.f90 \
		netcdf_overloads.f90 \
		netcdf_test.f90 \
		netcdf_text_variables.f90 \
		netcdf_variables.f90 \
		netcdf_visibility.f90 \
		nvea.m4 nveaput48.m4 nveaget48.m4\
		nves.m4 nvesput48.m4 nvesget48.m4 \
		typeSizes.f90 install-sh

all:            $(LIBRARY)

test:           netcdf_test
	./netcdf_test
	@$(NCDUMP) example.nc >example.cdl; \
	if cmp example_good.cdl example.cdl; then \
	    echo '*** Success ***'; \
	else \
	    echo '*** Failure ***'; \
	    diff -c example_good.cdl example.cdl; \
	fi

netcdf_test:    netcdf_test.f90 netcdf.o typeSizes.o
	$(LINK.F90) netcdf_test.f90 netcdf.o typeSizes.o $(LIBRARY) $(F90LIBS)

testEightByteGet:       testEightByteGet.o typeSizes.o $(LIBRARY)
	$(LINK.F90) $@.o $(F90LIBS) typeSizes.o $(LIBRARY)

EightByteIntTest:       EightByteIntTest.o typeSizes.o
	$(LINK.F90) $@.o typeSizes.o $(F90LIBS)

EightByteIntTest-2:     EightByteIntTest-2.o typeSizes.o
	$(LINK.F90) $@.o typeSizes.o $(F90LIBS)

typeSizes.o:    typeSizes.f90
	$(COMPILE.F90) typeSizes.f90

netcdf_expanded.f90:    gen.m4 f90aux.m4 nves.m4 nvea.m4 \
			nvesput48.m4 nvesget48.m4 nveaput48.m4 nveaget48.m4
	$(M4) gen.m4 >$@

netcdf.o:       \
	    netcdf.f90 typeSizes.o netcdf_constants.f90 netcdf_externals.f90 \
	    netcdf_overloads.f90 netcdf_visibility.f90 netcdf_file.f90 \
	    netcdf_dims.f90 netcdf_attributes.f90 netcdf_variables.f90 \
	    netcdf_text_variables.f90 netcdf_expanded.f90
	$(COMPILE.F90) netcdf.f90

install:        installed_netcdf_module installed_typesizes_module \
		$(MANDIR)/man3f90/$(MANUAL)

installed_netcdf_module:
	@if test -f netcdf.mod; then \
	    header=netcdf.mod; \
	elif test -f NETCDF.mod; then \
	    header=NETCDF.mod; \
	elif test -f NETCDF.MOD; then \
	    header=NETCDF.MOD; \
	elif test -f netcdf.MOD; then \
	    header=netcdf.MOD; \
	elif test -f netcdf.d; then \
	    header=netcdf.d; \
	else \
	    echo 1>&2 "netCDF header-module not found"; \
	    exit 1; \
	fi; \
	$(MAKE) HEADER=$$header $(INCDIR)/$$header

installed_typesizes_module:
	@if test -f typesizes.mod; then \
	    header=typesizes.mod; \
	elif test -f typeSizes.mod; then \
	    header=typeSizes.mod; \
	elif test -f TYPESIZES.mod; then \
	    header=TYPESIZES.mod; \
	elif test -f TYPESIZES.MOD; then \
	    header=TYPESIZES.MOD; \
	elif test -f typeSizes.MOD; then \
	    header=typeSizes.MOD; \
	elif test -f typesizes.MOD; then \
	    header=typesizes.MOD; \
	elif test -f typesizes.d; then \
	    header=typesizes.d; \
	else \
	    echo 1>&2 "typeSizes header-module not found"; \
	    exit 1; \
	fi; \
	$(MAKE) HEADER=$$header $(INCDIR)/$$header

uninstall:
	-rm -f \
	    $(INCDIR)/netcdf.mod \
	    $(INCDIR)/NETCDF.mod \
	    $(INCDIR)/NETCDF.MOD \
	    $(INCDIR)/netcdf.MOD \
	    $(INCDIR)/netcdf.d \
	    $(INCDIR)/typesizes.mod \
	    $(INCDIR)/typeSizes.mod \
	    $(INCDIR)/TYPESIZES.mod \
	    $(INCDIR)/TYPESIZES.MOD \
	    $(INCDIR)/typeSizes.MOD \
	    $(INCDIR)/typesizes.MOD \
	    $(INCDIR)/typesizes.d \
	    $(MANDIR)/man3f90/$(MANUAL)

include ../rules.make

.SUFFIXES:      .o .f90

.f90.o:
	$(COMPILE.F90) $<

EightByteIntTest.o:     typeSizes.o
EightByteIntTest-2.o:   typeSizes.o
