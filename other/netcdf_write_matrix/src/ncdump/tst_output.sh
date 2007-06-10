#!/bin/sh
# This shell script tests the output several previous tests.
# $Id: tst_output.sh,v 1.5 2005/11/07 20:25:32 ed Exp $

echo "*** Testing ncgen and ncdump test output for classic format."
set -e
echo "*** creating ctest1.cdl from ctest0.nc..."
./ncdump -n c1 ctest0.nc > ctest1.cdl
echo "*** creating c0.nc from c0.cdl..."
../ncgen/ncgen -b -o c0.nc $srcdir/../ncgen/c0.cdl
echo "*** creating c1.cdl from c0.nc..."
./ncdump -n c1 c0.nc > c1.cdl
echo "*** comparing ncdump of C program output (ctest1.cdl) with c1.cdl..."
diff c1.cdl ctest1.cdl

echo "*** All ncgen and ncdump test output for classic format passed!"

echo "*** Testing ncgen and ncdump test output for 64-bit offset format."
echo "*** creating ctest1.cdl from test0_64.nc..."
./ncdump -n c1 ctest0_64.nc > ctest1_64.cdl
echo "*** creating c0.nc from c0.cdl..."
../ncgen/ncgen -v2 -b -o c0.nc $srcdir/../ncgen/c0.cdl
echo "*** creating c1.cdl from c0.nc..."
./ncdump -n c1 c0.nc > c1.cdl
echo "*** comparing ncdump of C program output (ctest1_64.cdl) with c1.cdl..."
diff c1.cdl ctest1_64.cdl

echo "*** All ncgen and ncdump test output for 64-bit offset format passed!"
exit 0
