#!/bin/csh -f
echo output to wps.log
( wps.csh ; cd wrfv2_fire/test/em_real ; wrf.exe ) >& wps.log
