# Makefile for netCDF (semi)exhaustive test.
#
# $Id: Makefile,v 1.38 2006/01/30 18:56:02 ed Exp $

include ../macros.make
# M4FLAGS         = -s -B7168
# CC		= cc -fullwarn -woff 1209,1506

INCLUDES	= -I../libsrc -I..

ld_math		= $(MATHLIB)

SRCS		=   nc_test.c \
		    error.c \
		    test_get.c \
		    test_put.c \
		    test_read.c \
		    test_write.c \
		    util.c

OBJS		= $(SRCS:.c=.o)

lib_netcdf	= ../libsrc/libnetcdf.a
ld_netcdf	= -L../libsrc -lnetcdf

time_log	= times

GARBAGE		= nc_test nc_test_classic.nc nc_test_64bit.nc nc_test_netcdf4.nc scratch.nc lint.out $(time_log) large_files quick_large_files

PACKING_LIST	=  $(SRCS) \
		    test_get.m4 \
		    test_put.m4 \
		    error.h \
	            tests.h \
	            quick_large_files.c \
	            large_files.c \
		    depend	\
		    Makefile

MY_CFLAGS = -g -I../libsrc
MY_LDFLAGS = -L../libsrc
MY_LDLIBS = -lnetcdf

all:		nc_test

test:		nc_test
	./nc_test
	@echo '*** Success ***'

install:

uninstall:


nc_test:		$(OBJS) $(lib_netcdf)
	$(LINK.c) $(OBJS) $(lib_netcdf) $(ld_math) $(LIBS)

test_get.c:	test_get.m4

test_put.c:	test_put.m4

nctime:		nctime.o $(lib_netcdf)
	$(LINK.c) nctime.o $(ld_netcdf) $(ld_math) $(LIBS) 

time:	nctime
	time ./nctime 24 13 19 17 > $(time_log)
	awk -f timesum.awk < $(time_log)

sync_test:	nc_sync nc_sync_child
	PATH=$$PATH:. ./nc_sync

sync_programs:	nc_enddef nc_sync nc_sync_child

nc_enddef:	nc_enddef.o ../libsrc/libnetcdf.a
	$(LINK.c) -o $@ nc_enddef.o -L../libsrc -lnetcdf

nc_sync:	nc_sync.o ../libsrc/libnetcdf.a
	$(LINK.c) -o $@ nc_sync.o -L../libsrc -lnetcdf

nc_sync_child:	nc_sync_child.o ../libsrc/libnetcdf.a
	$(LINK.c) -o $@ nc_sync_child.o -L../libsrc -lnetcdf

quick_large_files: quick_large_files.c $(lib_netcdf)
	$(CC) $(CFLAGS) $(MY_CFLAGS) $(LDFLAGS) $(MY_LDFLAGS) $@.c -o $@ $(MY_LDLIBS)

large_files: large_files.c
	$(CC) $(CFLAGS) $(MY_CFLAGS) $(LDFLAGS) $(MY_LDFLAGS) $@.c -o $@ $(MY_LDLIBS)

# This tests whether large files can be written. It has nothing to do
# with netcdf. If you don't have /dev/zero this test will fail. If all
# the other large file tests pass, that's OK, but if any of them fail,
# you need to ensure that you can write very large files.
lfs_test:
	dd if=/dev/zero bs=1000000 count=3000 of=$(TEMP_LARGE)/largefile
	ls -l $(TEMP_LARGE)/largefile
	rm $(TEMP_LARGE)/largefile
	@echo '*** Success ***'

# This just does the quick large file tests. 
extra_test : quick_large_files
	./quick_large_files -f $(TEMP_LARGE)/example.nc
	-ls -l $(TEMP_LARGE)/example.nc 
	rm $(TEMP_LARGE)/example.nc
	@echo '*** Success ***'

# This does a slow test of a large file, by reading and then writing
# 4 GiB of data.
slow_test: large_files
	./large_files -f $(TEMP_LARGE)/example.nc
	-ls -l $(TEMP_LARGE)/example.nc
	rm $(TEMP_LARGE)/example.nc
	@echo '*** Success ***'

# Run all the large file tests we've got. Get a fresh cup of coffee
# and a donut, 'cause this takes a while!
all_large_tests: lfs_test extra_test slow_test

include ../rules.make
include depend
