Files in this directory are to read/write NETCDF files from matlab.

1. download the version of mexnc appropriate for your matlab from 
   http://mexcdf.sourceforge.net/downloads, suggest to this directory
2. uncompress and untar the file here so that mexnc is a subdirectory of this
directory

cd ../../..
cd wrfv2_fire/test/em_fire
<run wrf to produce some wrfrst* files> or put wrfrst files here
matlab
>> mpath                       % set the matlab path
>> f='wrfrst....'              % choose a netcdf file
>> p=ncdump(f)                 % list info on all variables
>> p=ncdump(f,'LFN')           % get info and value of one variable as
>> v=ncextract(p);             % extract variable value as Matlab array
>> lfn=ncread(f,'LFN');        % or, get the array directly 
>> ncload(f); mesh(lfn)        % alternatively, load all variables into the workspace

NOTE: in variable info, _nc is type used to read from netcdf, _m is the matlab
type corresponding to what is in the netcdf file. The type in the netcdf file
is determined from numerical values of the data type (vartype for variables
and datatype for attributes). The translation table is in private/ncdatatype.m
I am not sure if the table is correct because the numerical values for data
type are not documented. The documentation uses symbolic constants. 

