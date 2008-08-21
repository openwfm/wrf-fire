Files in this directory are to read/write NETCDF files from matlab.

1. download the version of mexnc appropriate for your matlab from 
   http://mexcdf.sourceforge.net/downloads, suggest to this directory
2. uncompress and untar the file
3. build mexnc  (cd mexnc; make worked for me)
4. put the mexnc directory and this directory on the matlab path

Usage:

cd wrf/wrfv2_fire/test/em_fire
<run wrf to produce some wrfrst files>
matlab
>> mpath                       % set the matlab path
>> f='wrfrst....'              % choose a netcdf file
>> v=nclist(f)                 % list info on all variables
>> [LFN,v]=ncread(f,'LFN'); v  % get one variable and its info

NOTE: I am not at all sure if the interpretation of the numerical values of
variable type (vartype) and attribute datatype (att_datatype) is correct.
This does not seem to be documented and the appropriate constants that  define
what the types mean are not available through the mexnc interface.
The translations returned in vartype_m and att_datatype_m are just a guess.

