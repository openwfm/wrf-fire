
cont=1
host=`hostname`
if [ ${host} = "wf.cudenver.edu" ] ; then
  base="/opt/wrf-libs"
elif [ ${host} = "opt4.cudenver.edu" ] ; then
  base="/home/grads/jbeezley/wrf-libs"
else
  echo "unknown host: " ${host}
  cont=0
fi

if [ $cont -eq 1 ] ; then
export NETCDF=${base}/netcdf
export JASPERLIB=${base}/jasper/lib
export JASPERINC=${base}/jasper/include
export NCARG=${base}/ncarg
export NCARG_ROOT=$NCARG
ulimit -s unlimited

ifvars=/opt/intel/fce/9.1.036/bin/ifortvars.sh
if [ -f ${ifvars} ] ; then
  source ${ifvars}
else
  echo "WARNING: couldn't find ifort setup script"
fi

else

echo "Quiting"
fi
