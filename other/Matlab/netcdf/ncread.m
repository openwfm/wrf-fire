function [out,varargout]=ncread(filename,varname)
% read variable, varname, from file, filename and output
% simplified interface to mexnc('get_var_double',...)

[ncid,status]=mexnc('OPEN',filename,'nowrite');
nccheck(status);
[varid,status]=mexnc('INQ_VARID',ncid,char(varname));
nccheck(status);
v=ncvarinfo(ncid,varid); % find out all about this variable
[data,status]=mexnc('get_var_double',ncid,varid);
nccheck(status);
out=permute(data,v.ndims:-1:1);
if(nargout>1),
	varargout{1}=v;
end
end
