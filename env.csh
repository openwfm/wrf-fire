
set cont=1
set host=`hostname`
set myhost=1
if ( ${host} == "wf.cudenver.edu" )  then
  set base="/opt/wrf-libs"
  set mpibins="/opt/intel9.1-64-par/bin"
else if ( ${host} == "opt4.cudenver.edu" ) then
  set base="/home/grads/jbeezley/wrf-libs"
  set mpibins="/home/grads/jbeezley/intel9.1-libs-par-64/bin"
else if ( ${host} == "walnut" ) then
  set base="/local"
  set myhost=0
else
  echo "unknown host: " ${host}
  set cont=0
endif

if ( $cont == 1 ) then

if ( $myhost == 1 ) then

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
setenv PATH ${mpibins}:${PATH}

else

setenv NETCDF ${base}/netcdf
setenv NCARG ${base}/ncarg
setenv NCARG_ROOT $NCARG
unlimit 

setenv PGI /usr/pgi
setenv PATH $PGI/linux86-64/6.2/bin:$PATH
setenv MANPATH $PGI/linux86-64/6.2/man
setenv LM_LICENSE_FILE $PGI/license.dat
setenv FC pgf77
setenv F90 pgf90

endif

else

echo "Quiting"
endif
