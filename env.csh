
set cont=1
set host=`hostname`
if ( ${host} == "wf.cudenver.edu" )  then
  set base="/opt/wrf-libs"
else if ( ${host} == "opt4.cudenver.edu" ) then
  set base="/scratch0/jbeezley/wrf-libs"
else
  echo "unknown host: " ${host}
  set cont=0
endif

if ( $cont == 1 ) then
setenv NETCDF ${base}/netcdf
setenv JASPERLIB ${base}/jasper/lib
setenv JASPERINC ${base}/jasper/include
setenv NCARG_ROOT ${base}/ncarg
setenv NCARG $NCARG_ROOT
unlimit

set ifvars=/opt/intel/fce/9.1.036/bin/ifortvars.csh
if ( -f ${ifvars} ) then
  source ${ifvars}
else
  echo "WARNING: couldn't find ifort setup script"
endif

else

echo "Quiting"
endif
