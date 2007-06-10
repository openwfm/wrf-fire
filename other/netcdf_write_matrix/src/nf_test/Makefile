# Makefile for netCDF (semi)exhaustive FORTRAN test.
#
# $Id: Makefile,v 1.16 2006/02/02 18:44:21 ed Exp $

include ../macros.make

# M4FLAGS	= -s -B7168
# CC		= cc -fullwarn -woff 1209,1506


INCLUDES	= 

lib_netcdf	= ../libsrc/libnetcdf.a
# Don't use "-L../libsrc -lnetcdf" in the following because that doesn't
# work on a CRAY T90 (sigh).
ld_netcdf	= $(lib_netcdf)

M4SRCS		= \
		  test_get.m4	\
		  test_put.m4
#

FSRCS		= \
		  $(M4SRCS:.m4=.F)	\
		  nf_error.F	\
		  nf_test.F	\
		  test_read.F	\
		  test_write.F	\
		  util.F
#

CSRCS		= fortlib.c

OBJS		= \
		  test_get.o	\
		  test_put.o	\
		  nf_error.o	\
		  nf_test.o	\
		  test_read.o	\
		  test_write.o	\
		  util.o	\
		  fortlib.o

time_log	= times

GARBAGE		= \
		  $(time_log)	\
		  lint.out	\
		  nf_test	\
		  scratch.nc	\
		  ftest		\
		  ftest.f77	\
		  ftest.nc       \
		  *.f77
#

PACKING_LIST	= \
		  $(CSRCS)	\
		  $(FSRCS)	\
		  $(M4SRCS)	\
		  depend	\
		  ftest.F	\
		  fills.nc	\
		  Makefile	\
	          tests.inc
#

ftest:		ftest.o $(lib_netcdf)
	$(LINK.F) ftest.o fortlib.o $(ld_netcdf) $(FLIBS) $(LIBS)

all:		nf_test

test:		nf_test ftest
	./nf_test
	./ftest

readonly:	nf_test test.nc
	./nf_test -r

install:

uninstall:


nf_test:	$(OBJS) $(lib_netcdf)
	$(LINK.F) $(OBJS) $(ld_netcdf) $(FLIBS) $(MATHLIB) $(LIBS)


tags:		$(FSRCS) $(CSRCS) FORCE
	ctags -t $(FSRCS) $(CSRCS) ../fortran/*.c ../libsrc/*.c

include ../rules.make

test_get.F:	test_get.m4
test_put.F:	test_put.m4

include depend
