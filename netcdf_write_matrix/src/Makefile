# $Id: Makefile,v 1.76 2006/02/26 16:43:59 ed Exp $


include macros.make


PACKAGE		= netcdf
SUBDIRS		= libsrc nctest nc_test fortran f90 nf_test ncgen ncdump man cxx win32
DIST_GARBAGE	= \
		config.cache	\
		config.status	\
		MANIFEST
PACKING_LIST	= \
		INSTALL	\
		COPYRIGHT	\
		MANIFEST	\
		Makefile	\
		README		\
		RELEASE_NOTES	\
		VERSION		\
		aclocal.m4	\
		configure	\
		configure.in	\
		macros.make.in	\
		config.guess	\
		config.sub	\
		install-sh	\
		ncconfig.in     \
		rules.make

# I moved fortran to the bottom of the list, sometimes AIX make get's confused 
# if the C library and the fortran library are built in the same second. - Ed
all:		libsrc/all	\
		ncgen/all	\
		ncdump/all	\
		cxx/all \
		fortran/all	\
		f90/all	

test:		fortran/all \
		libsrc/test \
		nc_test/test nctest/test \
		nf_test/test fortran/test \
		ncdump/all ncgen/all f90/test \
		ncgen/test ncdump/test cxx/test 

# This runs the quick large file tests.
extra_test:	all
		cd ncgen; $(MAKE) extra_test; cd ../..
		cd nc_test; $(MAKE) extra_test; cd ../..

# This runs the slow large file tests.
slow_test:      all
		cd nc_test; $(MAKE) lfs_test; cd ../..

# Check is the same as test.
check:		test
extra_check:    extra_test
slow_check:     slow_test

install:	all \
		libsrc/install \
		fortran/install \
		f90/install \
		ncgen/install \
		ncdump/install \
		cxx/install

uninstall:	libsrc/uninstall \
		fortran/uninstall \
		f90/uninstall \
		ncgen/uninstall \
		ncdump/uninstall \
		cxx/uninstall

clean:		libsrc/clean \
		nctest/clean nc_test/clean \
		fortran/clean f90/clean nf_test/clean \
		ncdump/clean ncgen/clean cxx/clean

distclean:	libsrc/distclean \
		nctest/distclean nc_test/distclean \
		fortran/distclean f90/distclean nf_test/distclean \
		ncdump/distclean ncgen/distclean cxx/distclean

libsrc/all	\
libsrc/test	\
libsrc/install	\
libsrc/uninstall	\
libsrc/clean	\
libsrc/distclean	\
nctest/all	\
nctest/test	\
nctest/clean	\
nctest/distclean	\
nc_test/all	\
nc_test/test	\
nc_test/clean	\
nc_test/distclean	\
fortran/clean	\
fortran/distclean	\
f90/clean	\
f90/distclean	\
nf_test/clean	\
nf_test/distclean	\
ncdump/all	\
ncdump/test	\
ncdump/install	\
ncdump/uninstall	\
ncdump/clean	\
ncdump/distclean	\
ncgen/all	\
ncgen/test	\
ncgen/install	\
ncgen/uninstall	\
ncgen/clean	\
ncgen/distclean	\
cxx/clean	\
cxx/distclean	\
:
	@subdir=`echo $@ | sed 's,/.*,,'`; \
	target=`echo $@ | sed 's,.*/,,'`; \
	$(MAKE) SUBDIR=$$subdir TGET=$$target subdir_target

fortran/all	\
fortran/test	\
fortran/install	\
fortran/uninstall	\
nf_test/all	\
nf_test/test	\
:
	@case "$(FC)" in \
	    '') echo 1>&2 "Not making \`$@' because no FORTRAN compiler"; \
		;; \
	    *)  subdir=`echo $@ | sed 's,/.*,,'`; \
		target=`echo $@ | sed 's,.*/,,'`; \
		$(MAKE) SUBDIR=$$subdir TGET=$$target subdir_target; \
		;; \
	esac

f90/all	\
f90/test	\
f90/install	\
f90/uninstall	\
:
	@case "$(F90)" in \
	    '') echo 1>&2 "Not making \`$@' because no FORTRAN-90 compiler"; \
		;; \
	    *)  subdir=`echo $@ | sed 's,/.*,,'`; \
		target=`echo $@ | sed 's,.*/,,'`; \
		$(MAKE) SUBDIR=$$subdir TGET=$$target subdir_target; \
		;; \
	esac

cxx/all	\
cxx/test	\
cxx/install	\
cxx/uninstall	\
:
	@case "$(CXX)" in \
	    '') echo 1>&2 "Not making \`$@' because no C++ compiler"; \
		;; \
	    *)  subdir=`echo $@ | sed 's,/.*,,'`; \
		target=`echo $@ | sed 's,.*/,,'`; \
		$(MAKE) SUBDIR=$$subdir TGET=$$target subdir_target; \
		;; \
	esac

subdir_target:
	@echo ""
	@cd $(SUBDIR) && \
	    echo "Making \`$(TGET)' in directory `pwd`" && \
	    echo "" && \
	    $(MAKE) $(TGET) || exit 1
	@echo ""
	@echo "Returning to directory `pwd`"
	@echo ""

install:	whatis
whatis:		$(MANDIR)/$(WHATIS)
$(MANDIR)/$(WHATIS):	$(MANDIR)
	$(MAKEWHATIS_CMD)
uninstall:
	rm -f $(MANDIR)/$(WHATIS)


################################################################################
# Distribution:

# Make a compressed, tar(1) file of the source distribution in the current 
# directory.
#

tar.Z:		FORCE
	@version=`cat VERSION`; \
	    $(MAKE) $(MFLAGS) $(PACKAGE)-$$version.tar.Z VERSION=$$version

$(PACKAGE)-$(VERSION).tar.Z:	ensure_manifest MANIFEST
	id=$(PACKAGE)-$(VERSION) \
	&& rm -rf $$id \
	&& mkdir $$id \
	&& ln -s .. $$id/src \
	&& tar $(TARFLAGS) - `sed "s|^|$$id/src/|" MANIFEST` | $(COMPRESS) > $@ \
	&& rm -r $$id

tar.gz:		FORCE
	@version=`cat VERSION`; \
	    $(MAKE) $(MFLAGS) $(PACKAGE)-$$version.tar.gz VERSION=$$version

$(PACKAGE)-$(VERSION).tar.gz:	ensure_manifest MANIFEST
	id=$(PACKAGE)-$(VERSION) \
	&& rm -rf $$id \
	&& mkdir $$id \
	&& ln -s .. $$id/src \
	&& tar $(TARFLAGS) - `sed "s|^|$$id/src/|" MANIFEST` | $(ZIP) > $@ \
	&& rm -r $$id

MANIFEST:	FORCE
	$(MAKE) MANIFEST.echo >$@

# This isn't to install docs, it's to make the INSTALL doc.
INSTALL: man/netcdf-install.texi
	makeinfo -I man -o INSTALL --no-headers man/netcdf-install.texi

# Make a compressed, tar(1) file of the source distribution in the
# appropriate FTP directory.
#
# NOTE: Making "ftp" will cause the "tar.gz" file to be made even if it
# already exists.  This is because the MANIFEST file upon which it
# depends must be remade every time.  This is not a waste of time,
# however, if the "tar.gz" target is made in private directories and the
# "ftp" target is made in the "/upc/$(PACKAGE)/build/" directory.
#
ftp:		FORCE
	version=`cat VERSION`; \
	$(MAKE) $(MFLAGS) $(FTPDIR)/$(PACKAGE)-$$version.tar.gz \
	    VERSION=$$version; \
	test -r $(FTPDIR)/$(PACKAGE).tar.gz || exit 0; \
	    cd $(FTPDIR) || exit 1;

$(FTPDIR)/$(PACKAGE)-$(VERSION).tar.gz:	$(PACKAGE)-$(VERSION).tar.gz
	rm -f $@
	cp $(PACKAGE)-$(VERSION).tar.gz $@ 
	chmod u+rw,g+rw,o=r $@

# Make a compressed, tar(1) file of the binary distribution in the 
# appropriate FTP directory.
#
binftp:		FORCE
	-mkdir $(FTPBINDIR)
	version=`cat VERSION`; \
	$(MAKE) $(MFLAGS) $(FTPBINDIR)/$(PACKAGE)-$$version.tar.Z \
	    VERSION=$$version; \
	$(MAKE) $(MFLAGS) $(FTPBINDIR)/$(PACKAGE)-$$version.env \
	    VERSION=$$version
ftpbin:		binftp

$(FTPBINDIR)/$(PACKAGE)-$(VERSION).tar.Z:
	rm -f $@
	id=$(PACKAGE)-$(VERSION) \
	&& rm -f $$id \
	&& ln -s $(prefix) $$id \
	&& tar $(TARFLAGS) - README $$id/bin $$id/include \
	    $$id/lib $$id/man | compress > $@ \
	&& rm $$id
	chmod u+rw,g+rw,o=r $@
	test -r $(FTPBINDIR)/$(PACKAGE).tar.Z || exit 0; \
	    cd $(FTPBINDIR) || exit 1;

$(FTPBINDIR)/$(PACKAGE)-$(VERSION).env:
	cp macros.make $@


include rules.make
