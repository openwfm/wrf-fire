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

#dx=30
#dy=30
rmesh="21 42 84 168"
datadir="${HOME}/wrffire_data/omp"
namelistvar="namelist.input.vars"
nthreads="1 2 4 8"

function setup_namelist
{
  rm -fr namelist.input &> /dev/null
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
	 > namelist.input

}

function runwrf
{
  time ./wrf.exe
}


export MP_STACK_SIZE=1000000000
for j in $nthreads ; do

  export OMP_NUM_THREADS=$j

for i in $rmesh ; do
  mesh_x=$i
  mesh_y=$i
  setup_namelist
  datadir1="${datadir}/mesh_m${i}_t${j}"
  rm -fr $datadir1 &> /dev/null
  mkdir -p $datadir1
  ./ideal.exe &> ideal.log
  runwrf &> run.log
  echo "OMP_NUM_THREADS=$OMP_NUM_THREADS" >> run.log
  mv wrfout_d* wrfrst_d* namelist.input run.log ideal.log $datadir1
done
done

  

