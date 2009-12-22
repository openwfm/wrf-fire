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
% if varname is given and string:
% get structure with attributes and value for one variable
%
% if varname is a cell array, return array of structures of the same shape


% Jan Mandel, September 2008
% developed from an earlier code by Jon Beezley

if ~exist('varname','var'),
    var=nclist(filename);
else
    if ischar(varname),
        if strcmp(varname,'-q'),
            var=nclist(filename,'-q');
        else
            var=ncvar(filename,varname);
        end
    elseif iscell(varname),
            for j=size(varname,2):-1:1,
                for i=size(varname,1):-1:1,
                    var(i,j)=ncvar(filename,varname{i,j});
                end
            end
    else
        varname,error('unsupported type')
    end
end
end


