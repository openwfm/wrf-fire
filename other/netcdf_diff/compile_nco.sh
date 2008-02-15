#!/bin/bash

tar zxf nco.tar.gz
pushd nco-3.9.3
F77=ifort NETCDF_INC=$NETCDF/include NETCDF_LIB=$NETCDF/lib ./configure --disable-dap --disable-netcdf4 --disable-i18n--enable-regex --enable-nco_cplusplus=no  --enable-shared=no --prefix=$PWD/.. --enable-ncoxx=no && make && make install 
popd
if ! [ -f bin/ncdiff ] 
then
  echo "failed to build nco"
  exit 1
fi
cp -f bin/ncdiff .

make netcdf_diff.x
