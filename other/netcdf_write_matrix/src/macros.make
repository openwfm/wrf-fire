# $Id: macros.make.in,v 1.40 2006/02/15 15:51:25 ed Exp $

# The purpose of this file is to contain common make(1) macros.
# It should be processed by every execution of that utility.


# POSIX shell.  Shouldn't be necessary -- but is under IRIX 5.3.
SHELL		= /bin/sh


# Installation Directories:
SRCDIR		= /home/jbeezley/netcdf_write_matrix/src
prefix		= /home/jbeezley
exec_prefix	= $(prefix)
INCDIR		= $(exec_prefix)/include
LIBDIR		= $(exec_prefix)/lib
BINDIR		= $(exec_prefix)/bin
MANDIR		= $(prefix)/man


# Preprocessing:
M4		= m4
M4FLAGS		= -B10000
CPP		= cc -E
CPPFLAGS	= $(INCLUDES) $(DEFINES) 
FPP		= 
FPPFLAGS	= 
CXXCPPFLAGS	= $(CPPFLAGS)


# Compilation:
CC		= cc
CXX		= c++
FC		= ifort
F90		= ifort
CFLAGS		= -DpgiFortran -g
CXXFLAGS	= -g -O2
FFLAGS		= -nofor_main 
F90FLAGS	= 
CC_MAKEDEPEND	= false
COMPILE.c	= $(CC) -c $(CFLAGS) $(CPPFLAGS)
COMPILE.cxx	= $(CXX) -c $(CXXFLAGS) $(CXXCPPFLAGS)
COMPILE.f	= $(FC) -c $(FFLAGS)
COMPILE.F90	= $(F90) -c $(F90FLAGS)
# The following command isn't available on some systems; therefore, the
# `.F.o' rule is relatively complicated.
COMPILE.F	= $(COMPILE.f) $(FPPFLAGS)


# Linking:
MATHLIB		= -lm
FLIBS		= 
F90LIBS		= 
LIBS		= 
F90LDFLAGS	= $(LDFLAGS)
LINK.c		= $(CC) -o $@ $(CFLAGS) $(LDFLAGS)
LINK.cxx	= $(CXX) -o $@ $(CXXFLAGS) $(LDFLAGS)
LINK.F		= $(FC) -o $@ $(FFLAGS) $(FLDFLAGS)
LINK.f		= $(FC) -o $@ $(FFLAGS) $(FLDFLAGS)
LINK.F90	= $(F90) -o $@ $(F90FLAGS) $(F90LDFLAGS)


# Manual pages:
WHATIS		= whatis
# The following macro should be empty on systems that don't
# allow users to create their own manual-page indexes.
MAKEWHATIS_CMD	= 


# Misc. Utilities:
AR		= ar
ARFLAGS		= cru
RANLIB		= ranlib
TARFLAGS	= -chf


# Dummy macros: used only as placeholders to silence GNU make.  They are
# redefined, as necessary, in subdirectory makefiles.
HEADER		= dummy_header
HEADER1		= dummy_header1
HEADER2		= dummy_header2
HEADER3		= dummy_header3
LIBRARY		= dummy_library.a
MANUAL		= dummy_manual
PROGRAM		= dummy_program


# Distribution macros. Since /home/ftp is no longer mounted
# everywhere, I have changed it to /home/ed/pub. After generating all
# binaries, I will tar the directory and scp it to the ftp directory on
# conan - Ed 2/16/6
FTPDIR		= /home/ed/pub/$(PACKAGE)
FTPBINDIR	= /home/ed/pub/binary/dummy_system
VERSION		= dummy_version
COMPRESS        = compress
ZIP        = gzip
INSTALL		= /usr/bin/install -c
# TEMP_LARGE is where the very large files are written.
# override this on the command line like this:
#  make TEMP_LARGE=/tmp extra_test
TEMP_LARGE = .
