#!/bin/bash

# used to export variables from netcdf output file

# /path/to/ncdump
NCDUMP_BIN="$1"

# the netcdf output file to use
NC_FILE="$2"

# directory to put files into
OUT_DIR="$3"

NUM_ARGS=3

# variables to export
VARS="XFG
YFG
XCD
YCD"

# usage statement
function usage() {
echo "usage:"
echo `basename $0` /path/to/ncdump /path/to/netcdf_file /path/to/output_dir
}

if [ $# -ne 3 ] ; then
  usage
  exit 1
fi

# check ncdump binary

if ! $NCDUMP_BIN 2>&1 | grep write_matrix &> /dev/null ; then
  echo "ncdump does not support -w flag"
  exit 1
fi

# check netcdf file

if [ ! -f $NC_FILE ] ; then
  echo "$NC_FILE is not a file"
  usage
  exit 1
fi

# test netcdf file
for var in $VARS ; do
  if ! $NCDUMP_BIN -h $NC_FILE 2>&1 | grep $var &> /dev/null ; then
    echo "either $NC_FILE is not a netcdf file or"
    echo "$var does not exist in the file"
    exit 1
  fi
done

if [ -z "$OUT_DIR" ] ; then
  echo "no output directory given"
  usage
  exit 1
fi

if [ -f "$OUT_DIR" ] ; then
  echo "$OUT_DIR is a file, not a directory"
  usage
  exit 1
fi

if [ -d "$OUT_DIR" ] && ls "${OUT_DIR}/*" &> /dev/null ; then
  echo "$OUT_DIR is not empty"
  usage
  exit 1
fi

# done with tests

# save full pathname of ncdump binary and netcdf file
NCDUMP_FULL=$(which $NCDUMP_BIN)
NETCDF_FULL=$(cd $(dirname "$NC_FILE") && echo $PWD)/$(basename "$NC_FILE")

if [ ! -e "$OUT_DIR" ] ; then
  mkdir -p "$OUT_DIR"
fi

cd "$OUT_DIR"
VC=""

for var in $VARS ; do 
  VC="${VC},$var"
done
  
$NCDUMP_FULL -w -v $VC $NETCDF_FULL &> ncdump.log
exit 0

