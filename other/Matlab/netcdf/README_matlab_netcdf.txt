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

works also in em_real

works as of commit f6e1299c976cc23224fd889ecf10fc1d7ac17855 Sep 14 2008

jm

NOTE: in variable info, _nc is type used to read from netcdf, _m is the matlab
type corresponding to what is in the netcdf file. The type in the netcdf file
is determined from numerical values of the data type (vartype for variables
and datatype for attributes). The translation table is in private/ncdatatype.m
I am not sure if the table is correct because the numerical values for data
type are not documented. The documentation uses symbolic constants. 

The disadvantage is that wrf creates the netcdf files (the wrfrst* and wrfout*) files at most once a minute, and only the arrays that are in the state are in those files. So, another possibility is call subroutine write_array_m in the code and then read the files using Matlab/util1_jan/read_array_m.m. See other/Matlab/vis But that will not work properly when wrf runs in parallel, 
either OpenMP or MPI.

jm
