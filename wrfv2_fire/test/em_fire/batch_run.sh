#!/bin/bash

# defaults
run_m=10
time_step_int=0
time_step_frac=5
mesh_x=42
mesh_y=42
mesh_z=41
dx=60
dy=60
sr_x=10
sr_y=10

rmesh="21 42 84 168"
datadir="${HOME}/wrffire_data/omp_gentoo"
nthreads="1 2 4 8"
xgrid_size=$(echo "scale=9; $mesh_x * $dx" | bc)
ygrid_size=$(echo "scale=9; $mesh_y * $dy" | bc)
gnutime="/usr/bin/time"
firedir="$(pwd)"
testdir="${firedir}/test"
tfiles="ideal.exe wrf.exe input_sounding*"
namelistvar="${firedir}/namelist.input.vars"
nprocs=2
ncoreproc=4
ncores=$((nprocs * ncoreproc))

timefmt='%e %S %U %P %M %W %c %w %x'
timeheader='realtime kerneltime usertime percent memory nswap iswap nwait exit'
ltimefile="time.log"
gtimefile="${datadir}/time_all.log"
runs='runme.x'
dfiles='namelist.input run.log $ltimefile ideal.log wrfout* wrfrst*'

declare -a pida pjob
for ((i=0;i<$ncores;i++)) ; do
  pida[$i]=0
  pjob[$i]=-1
done

function setup_namelist
{
#  rm -fr namelist.input &> /dev/null
  dx=$(echo "scale=9; $xgrid_size / $mesh_x" |bc)
  dy=$(echo "scale=9; $ygrid_size / $mesh_y" |bc)
  cat $namelistvar | \
  sed -e "s/RUN_M/$run_m/" \
         -e "s/TIME_STEP_INT/$time_step_int/" \
	 -e "s/TIME_STEP_FRAC/$time_step_frac/" \
	 -e "s/MESH_X/$mesh_x/" \
	 -e "s/MESH_Y/$mesh_y/" \
	 -e "s/MESH_Z/$mesh_z/" \
	 -e "s/DX/$dx/" \
	 -e "s/DY/$dy/" \
         -e "s/SR_X/$sr_x/" \
	 -e "s/SR_Y/$sr_y/" \
	 > $1
}

function runscript
{
  local i lpid
  runsl=${testdirl}/${runs}
  sdata=${datadir}/${append}_\$$
  fdata=${datadir}/fail/${append}_\$$
  cat > $runsl  <<EOF
#!/bin/bash
cd ${testdirl}
./ideal.exe &> ideal.log
$gnutime -f '$timefmt' -o ${ltimefile}.tmp  ./wrf.exe &> run.log
tail -1 ${ltimefile}.tmp > $ltimefile
sed -i 's/%//g' $ltimefile
status=\$(cut -d " " -f 9 $ltimefile)

if [ \$status -eq 0 ] ; then
  rm -fr $sdata &> /dev/null
  mkdir -p $sdata &> /dev/null
  pushd $testdirl &> /dev/null
  mv $dfiles $sdata
  popd &> /dev/null
  if ! [ -f $gtimefile ] ; then
    echo $timeheader > $gtimefile
  fi
  cat $ltimefile >> $gtimefile

else
  rm -fr $fdata &> /dev/null
  mkdir -p $fdata &> /dev/null
  pushd $testdirl &> /dev/null
  mv $dfiles $fdata
  popd &> /dev/null
fi

exit $status
EOF
  chmod +x $runsl
  taskset -c ${pmask} $runsl &
  lpid=$!
  for ((i=0;i<$1;i++)) ; do
    pida[${pmaska[$i]}]=$lpid
    pjob[${pmaska[$i]}]=$2
  done

}

function setuptestdir
{
  local i
  testdirl=${testdir}/${append}
  rm -fr $testdirl &> /dev/null
  mkdir -p $testdirl
  pushd $testdirl &> /dev/null
  for i in $tfiles ; do
    ln -sf ${firedir}/$i .
  done
  popd &> /dev/null
  setup_namelist ${testdirl}/namelist.input
}

function getwaitlist
{
  local maxcpu i j
  maxcpu=$(( ($1 - 1) / $ncoreproc + 1))
  rthreads=$(( $1 - ($maxcpu -1) * $ncoreproc))
  startc=0
  startp=1000000
  for ((i=0;i<$ncores;i++)) ; do
    lp=0
    for ((j=$i;j<$1+$i;j++)) ; do
      lp=$((lp+pjob[$j]))
    done
    if [ $lp -lt $startp ] ; then
      startp=$lp
      startc=$i
    fi
  done
#  echo $startp $startc
  unset pwait pmask pmaska
  j=0
  dowait=1
  for ((i=0;i<$ncores ;i++)) ; do
    if [ $i -ge $startc ] && [ $i -le $(($startc + $1 - 1)) ] ; then
      pmaska[$j]=$i
      if [ ${pida[$i]} -gt 0 ] ; then
        pwait="$pwait ${pida[$i]}"
        dowait=0
      fi
      ((j++))
    fi
  done
  pmask="${startc}-$((startc+$1 - 1))"
#  echo $pmask
  if [ $dowait -eq 0 ] ; then
#    echo "$pwait"
    wait $pwait &> /dev/null
  fi
  return 0
}

ijob=-1
njob=0
#export MP_STACK_SIZE=1000000000
for j in $nthreads ; do

  export OMP_NUM_THREADS=$j

for i in $rmesh ; do
  ((ijob++))
  ((njob++))
  mesh_x=$i
  mesh_y=$i
  append="mesh_m${i}_t${j}"
  datadir1="${datadir}/${append}"
  setuptestdir
  jdir[$ijob]=$testdirl
  jt[$ijob]=$j
  getwaitlist $OMP_NUM_THREADS
  runscript $OMP_NUM_THREADS $ijob
done
done

