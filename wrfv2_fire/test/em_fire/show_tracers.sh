#!/bin/bash

# a script to produce visualizations of tracer variables from wrfrst* files
# will produce matlab and png figures in the directory ./figs
# also it will give write_matrix formatted files in vars_wrfrst* directories
# for each restart file present

# to run the visualization script manually, or to get more information
# see the directory 'other/Matlab/plot_tracer'

# if you run into problems see the file matlab.log for hints on what went wrong

OLDDISPLAY=$DISPLAY
export DISPLAY=
rm -fr figs &> /dev/null
start_dir=$PWD
git_head=$(git-rev-parse --git-dir)/..
matlab_head=${git_head}/other/Matlab
cd ${matlab_head}/plot_tracer
ln -sf ${start_dir}/wrfrst* .
matlab < go.m &> ${start_dir}/matlab.log 
for i in $(ls -d vars*) ; do
  if [ -d $i ] ; then
    mv $i $start_dir
  fi
done
mv figs $start_dir
cd $start_dir
export DISPLAY=$OLDDISPLAY
