# Makefile for netCDF (semi)exhaustive test.
#
# $Id: Makefile,v 1.21 2006/01/26 20:35:57 ed Exp $

include ../macros.make
# CC = cc -fullwarn -woff 1498,1692


INCLUDES	= -I../libsrc -I..
SRCS		= varget.c vargetg.c varput.c varputg.c vardef.c vartests.c \
		  vputget.c vputgetg.c driver.c cdftests.c dimtests.c rec.c \
		  atttests.c misctest.c add.c error.c emalloc.c val.c slabs.c
OBJS		= $(SRCS:.c=.o)
lib_netcdf	= ../libsrc/libnetcdf.a
ld_netcdf	= -L../libsrc -lnetcdf
time_log	= times
GARBAGE		= nctest lint.out test.nc test2.nc $(time_log) testfile.nc
PACKING_LIST	= \
		    add.c	\
		    add.h	\
		    atttests.c	\
		    cdftests.c	\
		    depend	\
		    dimtests.c	\
		    driver.c	\
		    emalloc.c	\
		    emalloc.h	\
		    error.c	\
		    error.h	\
		    Makefile	\
		    misctest.c	\
		    nctime.c	\
		    README	\
		    rec.c	\
		    slabs.c	\
		    testcdf.h	\
		    ref_nctest_classic.nc	\
		    ref_nctest_64bit_offset.nc	\
		    tests.h	\
		    timesum.awk	\
		    val.c	\
		    val.h	\
		    vardef.c	\
		    varget.c	\
		    vargetg.c	\
		    varput.c	\
		    varputg.c	\
		    vartests.c	\
		    vputget.c	\
		    vputgetg.c


all:		nctest

check: test

test:		nctest
	./nctest
	cmp nctest_classic.nc ref_nctest_classic.nc && cmp nctest_64bit_offset.nc ref_nctest_64bit_offset.nc

install:

uninstall:


nctest:		$(OBJS) $(lib_netcdf)
	$(LINK.c) $(OBJS) $(lib_netcdf) $(LIBS) $(ld_math)

nctime:		nctime.o $(lib_netcdf)
	$(LINK.c) $(CFLAGS) nctime.o $(ld_netcdf) $(LIBS) 

time:	nctime
	time ./nctime 24 13 19 17 > $(time_log)
	awk -f timesum.awk < $(time_log)

saber_src:
	#load -C $(CPPFLAGS) $(SRCS) $(ld_netcdf) $(LIBS)

include ../rules.make
include depend
