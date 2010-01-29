function ncreplace(filename,varname,value)
% ncreplace(filename,varname,value)
% replace variable value in netcdf file
% the number of entries in the variable must match
fprintf('ncreplace: open %s\n',filename)
ncid = netcdf.open(filename,'NC_WRITE');
fprintf('replacing variable %s\n',varname)
varid = netcdf.inqVarID(ncid,char(varname));
v=ncvarinfo(ncid,varid); % find out all about this variable
% transpose
% if ndims(value)>1,
    %v.dimlength=v.dimlength(v.ndims:-1:1);
	%value=permute(value,[ndims(value):-1:1]);
% end
[s,c]=getstartcount(v);
netcdf.putVar(ncid,varid,s,c,value);
netcdf.close(ncid);
nvalue=ncread(filename,varname);
err=big(abs(single(nvalue(:))-single(value(:)))./(abs(nvalue(:))+realmin));
v.var_value=value; % just to display the dimension
dispvarinfo(v);
if(err>1e-6),
    error('ncreplace','bad replaced value');
end
end
