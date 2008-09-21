
cont=1
host=`hostname`
myhost=1
if [ ${host} = "wf" ] ; then
  base="/opt/wrf-libs"
  source $base/setpaths.sh
  mpibins="/opt/intel9.1-64-par/bin"
elif [ ${host} = "opt4.cudenver.edu" ] ; then
  base="/home/grads/jbeezley/wrf-libs"
  mpibins="/home/grads/jbeezley/intel9.1-libs-par-64/bin"
elif [ ${host} = "walnut" ] ; then
  base="/local"
  myhost=0
else
  echo "unknown host: " ${host}
  cont=0
fi

if [ $cont -eq 1 ] ; then

if [ $myhost -eq 1 ] ; then
export NETCDF=${base}/netcdf
export JASPERLIB=${base}/jasper/lib
export JASPERINC=${base}/jasper/include
export NCARG=${base}/ncarg
export NCARG_ROOT=$NCARG
ulimit -s unlimited

#ifvars=/opt/intel/fce/9.1.036/bin/ifortvars.sh
ifvars=/opt/intel/fce/current/bin/ifortvars.sh
if [ -f ${ifvars} ] ; then
  source ${ifvars}
else
  echo "WARNING: couldn't find ifort setup script"
fi
export PATH=$mpibins:$PATH

else

export NETCDF=${base}/netcdf
export NCARG=${base}/ncarg
export NCARG_ROOT=$NCARG
ulimit -s unlimited

export PGI=/usr/pgi
export PATH=$PGI/linux86-64/6.2/bin:$PATH
export MANPATH=$PGI/linux86-64/6.2/man
export LM_LICENSE_FILE='7496@licenseb.ucar.edu:7496@licensea.ucar.edu'
export FC=pgf77
export F90=pgf90
export LAPACK='-L/usr/pgi/linux86-64/6.2/lib -llapack -lblas'

fi

else

echo "Quiting"
fi
