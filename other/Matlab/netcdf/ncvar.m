function v=ncvar(filename,varname,start,count)
% v=ncvar(filename,varname [,start,count])
% read all about variable varname from filename
% arguments:
%      filename   character, NetCDF file name
%      varname    character, variable name
%      start,count not present: read all  variable data 
%                 empty: do not read variable data 
%                 given: read in each dimension from start (begins at 0) count entries

read_value=1;
if exist('start','var')
    if isempty(start)
        read_value=0;
    else
        read_value=2;
    end
end
      
% fprintf('ncdump/ncvar: open %s\n',filename)
try
   ncid = netcdf.open(filename,'NC_NOWRITE');
catch ERR 
   disp(['cannot open NetCDF file ',filename])
   rethrow(ERR);
end
%fprintf('reading variable %s\n',varname)
varid = netcdf.inqVarID(ncid,char(varname));
v=ncvarinfo(ncid,varid); % find out all about this variable
if read_value==2,
    s=start;
    c=count;
else
    [s,c]=getstartcount(v);
end
if read_value>0,
    v.var_value = netcdf.getVar(ncid,varid,s,c);
end
netcdf.close(ncid);
dispvarinfo(v);
end

