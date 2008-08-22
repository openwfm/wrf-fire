Files in this directory are to read/write NETCDF files from matlab.

1. download the version of mexnc appropriate for your matlab from 
   http://mexcdf.sourceforge.net/downloads, suggest to this directory
2. uncompress and untar the file
3. build mexnc  (cd mexnc; make worked for me)
4. put the mexnc directory and this directory on the matlab path

Usage:

cd wrf/wrfv2_fire/test/em_fire
<run wrf to produce some wrfrst* files>
matlab
>> mpath                       % set the matlab path
>> f='wrfrst....'              % choose a netcdf file
>> v=nclist(f)                 % list info on all variables
>> [LFN,v]=ncread(f,'LFN'); v  % get one variable and its info

NOTE: in variable info, _nc is type used to read from netcdf, _m is the matlab
type corresponding to what is in the netcdf file. The type in the netcdf file
is determined from numerical values of the data type (vartype for variables
and datatype for attributes). The translation table is in private/ncdatatype.m
I am not sure if the table is correct because the numerical values for data
type are not documented. The documentation uses symbolic constants. 

