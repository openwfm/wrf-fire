#!/bin/sh
# This shell script runs the ncdump tests.
# $Id: run_tests.sh,v 1.11 2005/12/05 13:59:48 ed Exp $

echo "*** Testing ncgen and ncdump using test0.cdl."
set -e
echo "*** creating test0.nc from test0.cdl..."
../ncgen/ncgen -b $srcdir/test0.cdl
echo "*** creating test1.cdl from test0.nc..."
./ncdump -n test1 test0.nc > test1.cdl
echo "*** creating test1.nc from test1.cdl..."
../ncgen/ncgen -b test1.cdl
echo "*** creating test2.cdl from test1.nc..."
./ncdump test1.nc > test2.cdl
cmp test1.cdl test2.cdl
echo "*** All tests of ncgen and ncdump using test0.cdl passed!"
exit 0
