# Makefile for ncdump(1).
#
# $Id: Makefile,v 1.24 2006/01/27 23:00:56 ed Exp $

include ../macros.make

NCGEN		= ../ncgen/ncgen
INCLUDES	= -I../libsrc -I..
UTIL_LIB        = /home/jbeezley/wrf/fire_libs/libutil1_jan.a

c_sources	= ncdump.c vardata.c dumplib.c
headers		= ncdump.h vardata.h dumplib.h

PROGRAM		= ncdump

PACKING_LIST = $(c_sources) $(headers) depend test0.cdl ncdump.1	\
run_tests.sh tst_64bit.sh tst_output.sh tst_lengths.sh			\
rewrite-scalar.c Makefile install-sh small.cdl

MANUAL		= ncdump.1

lib_netcdf	= ../libsrc/libnetcdf.a

# Don't use "-L../libsrc -lnetcdf" in the following because that doesn't
# work on a CRAY T90 (sigh).
ld_netcdf	= $(lib_netcdf)

OBJS		=  ncdump.o vardata.o dumplib.o vardata_write.o
GARBAGE		= $(PROGRAM) test0.nc test1.nc test1.cdl test2.cdl

all:		$(PROGRAM)

$(PROGRAM):	$(lib_netcdf) $(OBJS) $(UTIL_LIB)
	$(LINK.f)  $(OBJS) $(ld_netcdf) $(LIBS) $(UTIL_LIB)

ctest.c:
	../ncgen/ncgen -c -o ctest0.nc ../ncgen/c0.cdl > ctest.c

ctest64.c:
	../ncgen/ncgen -v2 -c -o ctest0_64.nc ../ncgen/c0.cdl > ctest64.c

ctest: $(lib_netcdf) ctest.c
	$(LINK.c) ctest.c $(INCLUDES) $(ld_netcdf) $(LIBS) 

ctest64: $(lib_netcdf) ctest64.c
	$(LINK.c) ctest64.c $(INCLUDES) $(ld_netcdf) $(LIBS) 

rewrite-scalar: $(lib_netcdf) rewrite-scalar.c
	$(LINK.c) rewrite-scalar.c $(INCLUDES) $(ld_netcdf) $(LIBS) 

test:		$(PROGRAM) ctest ctest64 rewrite-scalar FORCE
	srcdir=. ./run_tests.sh
	srcdir=. ./tst_64bit.sh
	./ctest
	./ctest64
	srcdir=. ./tst_output.sh
	srcdir=. ./tst_lengths.sh
	     echo "*** $(PROGRAM) test successful ***"

install:	$(BINDIR)/$(PROGRAM) $(MANDIR)/man1/$(MANUAL)

uninstall:
	-rm -f $(BINDIR)/$(PROGRAM)
	-rm -f $(MANDIR)/man1/$(MANUAL)

$(PROGRAM)_oc : $(c_sources)
	#setopt primary_language C
	#load -C $(CPPFLAGS) $(c_sources)
	#load -C $(LIBS)
	#setopt program_name $(PROGRAM)

TAGS:		FORCE
	etags `echo $(PACKING_LIST) | fmt -1 | egrep '\.c|\.h'

include ../rules.make
include depend
