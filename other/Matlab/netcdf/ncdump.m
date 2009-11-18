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


