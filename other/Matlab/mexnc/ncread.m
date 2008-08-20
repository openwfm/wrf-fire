function out=ncread(filename,varname)
% read variable, varname, from file, filename and output
% simplified interface to mexnc('get_var_double',...)

thisdir=pwd;
[fpath,name,ext,version]=fileparts(char(filename));
cd(fpath);
lfile=[name ext version];

[ncid,status]=mexnc('open',lfile);
cd(thisdir);
check(status);

[varid,status]=mexnc('inq_varid',ncid,char(varname));
check(status);
[out,status]=mexnc('get_var_double',ncid,varid);
check(status);
return


function check(status)
if(status ~= 0),
    fstat=mexnc('strerror',status);
    error(fstat)
end
return