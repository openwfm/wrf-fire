function v=ncvar(filename,varname)
% simplified interface to mexnc('get_var_double',...)
fprintf('ncdump/ncvar: open %s\n',filename)
ncid = netcdf.open(filename,'NC_NOWRITE');
fprintf('reading variable %s\n',varname)
varid = netcdf.inqVarID(ncid,char(varname));
v=ncvarinfo(ncid,varid); % find out all about this variable
[s,c]=getstartcount(v);
value = netcdf.getVar(ncid,varid,s,c);
netcdf.close(ncid);
% permute dimensions - matlab stores columnwise, netcdf rowwise
if v.ndims>1,
    % v.dimlength=v.dimlength(v.ndims:-1:1);
    % value=permute(value,[v.ndims:-1:1]);
end
v.var_value=value;
dispvarinfo(v);
end

