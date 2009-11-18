function v=ncvar(filename,varname)
% simplified interface to mexnc('get_var_double',...)
fprintf('ncdump/ncvar: open %s\n',filename)
%[ncid,status]=mexnc('OPEN',filename,'nowrite');
%nccheck(status);
ncid = netcdf.open(filename,'NC_NOWRITE');
fprintf('reading variable %s\n',varname)
%[varid,status]=mexnc('INQ_VARID',ncid,char(varname));
%nccheck(status);
varid = netcdf.inqVarID(ncid,char(varname));
v=ncvarinfo(ncid,varid); % find out all about this variable
%[value,status]=mexnc(['GET_VAR_',v.vartype_nc],ncid,varid);
%nccheck(status);
value = netcdf.getVar(ncid,varid);
%status=mexnc('CLOSE',ncid);
%nccheck(status)
netcdf.close(ncid);
% permute dimensions - matlab stores columnwise, netcdf rowwise
if v.ndims>1,
    % v.dimlength=v.dimlength(v.ndims:-1:1);
    % value=permute(value,[v.ndims:-1:1]);
end
v.var_value=value;
dispvarinfo(v);
end

