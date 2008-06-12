#!/bin/bash

# regression testing for different configure flags

# dependent on arch configure defaults, as of this writing it is:
#
#   1.  (WF) x86_64 Linux, ifort+gcc compiler (single-threaded, no nesting) debug
#   2.  (WF) x86_64 Linux, ifort+gcc compiler (single-threaded, no nesting) -O2
#   3.  (WF) x86_64 Linux, ifort+gcc compiler (OpenMP) debug
#   4.  (WF) x86_64 Linux, ifort+gcc compiler (OpenMP) -O2
#   5.  (WF) x86_64 Linux, ifort+gcc compiler  DM-Parallel (RSL-LITE, MPICH, allows nesting) debug
#   6.  (WF) x86_64 Linux, ifort+gcc compiler  DM-Parallel (RSL-LITE, MPICH, allows nesting) -O2
#   7.  (WF) x86_64 Linux, ifort+gcc compiler  DM-Parallel+OMP (RSL-LITE, MPICH, allows nesting) debug
#   8.  (WF) x86_64 Linux, ifort+gcc compiler  DM-Parallel+OMP (RSL-LITE, MPICH, allows nesting) -O2
#   9.  (WALNUT) x86_64 Linux, PGI compiler (Single-threaded, no nesting) debug
#  10.  (WALNUT) x86_64 Linux, PGI compiler (Single-threaded, no nesting) -O2
#  11.  (WALNUT) x86_64 Linux, PGI compiler  SM-Parallel (OpenMP, no nesting) -O2
#  12.  (WALNUT) x86_64 Linux, PGI compiler  DM-Parallel (RSL_LITE, MPICH, Allows nesting) -O2

branch="jb/merge_test"
gitrepo="${USER}@math.cudenver.edu:/home/grads/jbeezley/wrf.git"
compiletarg="em_fire"
real="./ideal.exe"
wrf="./wrf.exe"
testdir="${HOME}/regtest_data"
nthreads=4
nprocs=4
gtime="/usr/bin/time"

CFGOPTS="1 2 3 4 5 6 7 8"
runtype[1]=1
runtype[2]=1
runtype[3]=3
runtype[4]=3
runtype[5]=5
runtype[6]=5
runtype[7]=6
runtype[8]=6

# 1: normal
# 2: nested
# 3: OMP
# 4: OMP+nested
# 5: mpich(+nested)
# 6: mpich+OMP(+nested)

renice +5 $$
odir=${PWD}
setenv=$(false)
source env.sh
j=0
mdp &> /dev/null &
rm -r ${testdir} &> /dev/null
mkdir -p ${testdir}
for i in $CFGOPTS ; do 
  ((j++))
  rm -fr $i &> /dev/null
  mkdir -p $i
  cd $i
  testdirl=${testdir}/$i
  mkdir -p ${testdirl}
  git clone ${gitrepo}
  cd wrf
  git merge origin/${branch}
  if [ $setenv ] ; then
    source "env.sh"
    setenv=$(false)
  fi
  cd wrfv2_fire
  echo $i | ./configure &> ${testdirl}/configure.log
  ./compile ${compiletarg} &> ${testdirl}/compile.log
  cd test/${compiletarg}

  wrfl=${wrf}
  if [ ${runtype[$j]} -eq 5 ] || [ ${runtype[$j]} -eq 6 ] ; then
    wrfl="mpirun -np $nproc $wrf < /dev/null "
  fi

  export OMP_NUM_THREADS=$nthreads
# standard run test
  echo "testing standard run with config option $i"
  cp ${odir}/namelist.input.std namelist.input
  ${real} &> ${testdirl}/real_std.log
  ${gtime} -o time_std.log ${wrf} &> ${testdirl}/run_std.log
  mkdir ${testdirl}/std
  mv time_std.log ${testdirl}
  mv wrfout_d* ${testdirl}/std
  mv rsl.* ${testdirl}/std &> /dev/null

  if [ ${runtype[$j]} -eq 2 ] || [ ${runtype[$j]} -eq 5 ] || [ ${runtype[$j]} -eq 6 ] ; then
    # nested test run
    echo "testing nested run with config option $i"
    cp ${odir}/namelist.input.nested namelist.input
    ${real} &> ${testdirl}/real_nest.log
    ${gtime} -o time_nest.log ${wrf} &> ${testdirl}/run_nest.log
    mkdir ${testdirl}/nest
    mv time_nest.log ${testdirl}
    mv wrfout_d* ${testdirl}/nest
    mv rsl.* ${testdirl}/nest &> /dev/null
  fi

  cd $odir
done






