#!/bin/bash

compiler=pgi
if [ $# -gt 0 ] ; then
  echo $1
  compiler=$1
fi

toplevel=${PWD}
gittop=${toplevel}/..
regdir=${gittop}/other/regressiontest
scratch=${HOME}/scratch
user=${USER}
hashid=$(git rev-parse HEAD)
logdir=${scratch}/${compiler}_regtest_${hashid}
ideal_nml=${regdir}/namelist.ideal
real_nml=${regdir}/namelist.real
sounding=${regdir}/input_sounding
ideal_fire_nml=${regdir}/namelist.fire.ideal
real_fire_nml=${regdir}/namelist.fire.real
metdata=${regdir}
nameprefix="std_"
numprocs=10
maxprocs=${numprocs}
maxcores=${numprocs}
maxthreads=${numprocs}
procscl=2
thrdscl=2
realout='wrfout_d01_2005-08-28_12:00:00'
idealout='wrfout_d01_0001-01-01_00:01:00'
ctimefile="${logdir}/compile_time.log"
rtimefile="${logdir}/run_time.log"
infofile="${logdir}/regtest_info.log"

if [ ${compiler} == pgi ] ; then
  export NETCDF=/opt/lib/pgi-11.9/netcdf-4.1.3
elif [ ${compiler} == gcc ] ; then
  export NETCDF=/opt/lib/gcc-4.4/netcdf-4.1.3
else
  echo Unknown compiler; $compiler 
  exit 1
fi
#export PATH=/opt/intel11-libs/bin:$PATH
ulimit -s unlimited > /dev/null
#mpd &> /dev/null &

if [ ! -f compile ] || [ ! -f configure ] ; then
  echo "not in toplevel directory!!!"
  exit 1
fi

function tic {
timer=$(date "+%s")
}

function start_ctime {
tic
ctime=$timer
}

function stop_ctime {
tic
((ctime=$timer - $ctime))
echo ${name} ${ctime}s >> $ctimefile
}

function start_rtime {
tic
rtime=$timer
}

function stop_rtime {
tic
((rtime=$timer - $rtime))
echo ${rname} ${rtime}s >> $rtimefile
}

function start_gtime {
tic
gtime=$timer
}

function stop_gtime {
tic
((gtime=$timer - $gtime))
echo "Total time taken: ${gtime}s" >> $infofile
}

function info_header {
start_gtime
cat > $infofile <<EOF
Starting regression test from: ${toplevel}
Date: $(date)
Host: $(hostname)
User: ${user}
$(./phys/commit_hash)
EOF
}

function info_footer {
cat >> $infofile<<EOF
Finished regression test
$(date)
EOF
stop_gtime
}

function clean_run_dir {
rm -fr met_em* wrfinput_d?? wrfbdy_d?? rsl.{out,error}.* &> /dev/null
}

function move_failed {
local f=$1
local e="$(echo ${f} | sed 's/\(log\|\)$/FAILED/')"
mv $f $e
echo $e >> $infofile
}

function run_wrf {
local p t n nproc nthread lout

if [ ${targ} = em_fire ] ; then
  lout="${idealout}"
else
  lout="${realout}"
fi

if [ $copt -eq 9 -o $copt -eq 1 ] ; then
  nproc=1
  nthread=1
  usempi=false
elif [ $copt -eq 10 -o $copt -eq 2 ] ; then
  nproc=1
  nthread=$maxthreads
  usempi=false
elif [ $copt -eq 11 -o $copt -eq 3 ] ; then
  nproc=$maxprocs
  nthread=1
  usempi=true
elif [ $copt -eq 12 -o $copt -eq 4 ] ; then
  nproc=$maxprocs
  nthread=$maxthreads
  usempi=true
else
  echo 'WHA?!?!?!'
  exit 1
fi
for ((p=1;p<=$nproc;p*=$procscl)) ; do
  for ((t=1;t<=$nthread ;t*=$thrdscl)) ; do
    ((n=p*t))
    if [ $n -le $maxcores ] ; then
      export OMP_NUM_THREADS=$t
      rname=$(printf '%s_p%02d_t%02d' "${name}" "${p}" "${t}")
      rlog="${rname}.log"
      if [ $usempi = true ] ; then
	start_rtime
        mpirun -np $p ./wrf.exe < /dev/null &> ${rlog}
	if [ ! -f "${lout}" ] ; then
           for i in rsl.{out,error}.* ; do
	     mv "$i" "${i}.ERROR"
	   done
	   move_failed "${rlog}"
	 fi
  	stop_rtime
	for i in rsl.{out,error}.* ; do
	  mv $i "${rname}.$i"
	done
      else
	start_rtime
        ./wrf.exe &> ${rlog}
	if [ ! -f "${lout}" ] ; then
	  move_failed "${rlog}"
	fi
        stop_rtime
      fi
      if [ -f "${lout}" ] ; then
	mv "${lout}" "${rname}.nc"
      fi
    fi
  done
done
}

function run_wrf_input {
  if [ $targ = em_fire ] ; then
    cp ${sounding} input_sounding
    cp ${ideal_nml} namelist.input
    cp ${ideal_fire_nml} namelist.fire
    ideallog="${name}_ideal.log"
    ./ideal.exe &> $ideallog
    if [ ! -f wrfinput_d01 ] ; then
      move_failed "${ideallog}"
    fi
    mv wrfinput_d01 wrfinput_ideal &> /dev/null
    mv wrfbdy_d01 wrfbdy_ideal &> /dev/null
  else
    cp ${real_nml} namelist.input
    cp ${real_fire_nml} namelist.fire
    reallog="${name}_real.log"
    ln -sf ${metdata}/met_em* .
    ./real.exe &> ${reallog}
    if [ ! -f wrfinput_d01 ] ; then
      move_failed "${reallog}"
    fi
    mv wrfinput_d01 wrfinput_real &> /dev/null
    mv wrfbdy_d01 wrfbdy_real &> /dev/null
  fi
}

function copynamelist_real {
  cp ${sounding} input_sounding
  cp ${real_nml} namelist.input
  cp ${real_fire_nml} namelist.fire
}

function copynamelist_ideal {
  cp ${sounding} input_sounding
  cp ${ideal_nml} namelist.input
  cp ${ideal_fire_nml} namelist.fire
}

function get_wrf_input {
if [ $targ = em_fire ] ; then
  if [ ! -f wrfinput_ideal ] ; then
    run_wrf_input
  fi
  copynamelist_ideal
  cp wrfinput_ideal wrfinput_d01 &> /dev/null
  cp wrfbdy_ideal wrfbdy_d01 &> /dev/null
else
  if [ ! -f wrfinput_real ] ; then
    run_wrf_input
  fi
  copynamelist_real
  cp wrfinput_real wrfinput_d01 &> /dev/null
  cp wrfbdy_real wrfbdy_d01 &> /dev/null
fi
}

mkdir -p ${logdir} &> /dev/null
if [ $(ls -1 ${logdir} | wc -l) -gt 0 ] ; then
  echo "The log directory ${logdir} is not empty"
  echo "stopping"
  exit 1
fi

if [ ! -d ${logdir} ] ; then
  echo "Couldn't create log directory"
  exit 1
fi

info_header
if [ $compiler == pgi ] ; then
  opts=(1 2 3 4)
fi
if [ $compiler == gcc ] ; then
  opts=(9 10 11 12)
fi
for copt in ${opts[*]} ; do

  for nest in 1 ; do

    for dbg in 0 1 ; do

      cd ${toplevel}
      ./clean -a &> /dev/null
      if [ $dbg -eq 1 ] ; then
	dflag=-d
      else
	dflag=' '
      fi
      echo "./configure $dflag $copt $net"
      ./configure $dflag > /dev/null <<EOF
$copt
$nest
EOF
      for targ in em_fire ; do

	name="${nameprefix}$(printf '%s_C%1d_N%1d_D%1d' $targ $copt $nest $dbg)"
	cname="${name}_compile.log"

        cp configure.wrf ${name}_configure.log
	start_ctime
	./compile $targ &> ${cname}

	if [ ! -f "test/${targ}/wrf.exe" ] ; then
	  
	  move_failed "${cname}"

	else

	  stop_ctime
          pushd "test/${targ}"
	  if [ -f ideal.exe ] ; then
	    cp ideal.exe ${name}_ideal.exe
	  fi
	  if [ -f real.exe ] ; then
	    cp real.exe ${name}_real.exe
	  fi
	  cp wrf.exe ${name}_wrf.exe
	  clean_run_dir
	  get_wrf_input
	  if [ -f wrfinput_d01 ] ; then
  	    run_wrf
	  fi
          mv *${name}* ${logdir} &> /dev/null
	  popd

	fi

	cd ${toplevel}
        mv *${name}* ${logdir} &> /dev/null

      done
    done
  done
done
info_footer

