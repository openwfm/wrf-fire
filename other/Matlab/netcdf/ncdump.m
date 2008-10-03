function var=ncdump(filename,varname)
% var=ncdump(filename)
%   get the list of all variables and print their info
%
% var=ncdump(filename,'-q')
%   get the list of all variables quietly
%
% var=ncdump(filename,varname)
%   read one variable in its native form and its info
%   (use ncread to read a variable as a Matlab matrix)
% 
% filename     the name of netcdf file
% varname      the name of a variable in the file
% var          returned structure with fields describing the variable
%
% if varname not given:
% get info for all variables in the netcdf file
%
% if varname is given:
% get info for one variable and its value

% Jan Mandel, September 2008
% developed from an earlier code by Jon Beezley

if ~exist('varname','var'),
    var=nclist(filename);
else
    if strcmp(varname,'-q'),
        var=nclist(filename,'-q');
    else
        var=ncvar(filename,varname);
    end
end
end

function var=nclist(filename,q) 
% get info on all variables

quiet=exist('q','var');
fprintf('file %s\n',filename)
[ncid,status] = mexnc('OPEN',filename,'nowrite');
nccheck(status)
[ndims,nvars, ngatts, unlimdim, status] = mexnc('INQ',ncid); % global info
nccheck(status)
for varid=1:nvars, % one variable at a time
    var(varid)=ncvarinfo(ncid,varid-1);
    if ~quiet,
        fprintf('%i ',varid);
        dispvarinfo(var(varid));
    end
end
status=mexnc('CLOSE',ncid);
nccheck(status)
end

function v=ncvar(filename,varname)
% simplified interface to mexnc('get_var_double',...)
fprintf('file %s\n',filename)
[ncid,status]=mexnc('OPEN',filename,'nowrite');
nccheck(status);
fprintf('file %s\n',varname)
[varid,status]=mexnc('INQ_VARID',ncid,char(varname));
nccheck(status);
v=ncvarinfo(ncid,varid); % find out all about this variable
[value,status]=mexnc(['GET_VAR_',v.vartype_nc],ncid,varid);
nccheck(status);
status=mexnc('CLOSE',ncid);
nccheck(status)
if v.ndims>1,
	value=permute(value,v.ndims:-1:1);
end
v.var_value=value;
dispvarinfo(v);
end

function dispvarinfo(p)
% display info on one variable
    disp([p.varname,' ',p.vartype_m,' (',num2str(p.dimlength),')'])
end
