WPS_PATH=../../../WPS
GEOGRID_PATH=$(WPS_PATH)/geogrid/src

all: WPSProj.so

WPSProj.so: libgeogrid.a
	f2py-2.6 --fcompiler=g95 -c projection.pyf simple_projection.F90 -I../../../WPS/geogrid/src -L. -lgeogrid

libgeogrid.a:
	ar -cr $@ $(GEOGRID_PATH)/*.o
	ranlib $@

