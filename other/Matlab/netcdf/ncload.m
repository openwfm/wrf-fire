function [varargout]=ncload(f)
% ncload(f)
% load all variables from a netcdf file f to base namespace
% put their names in lowercase
% 
% p=ncload(f)
% create structure with values of arrays in file f
%
% Jan Mandel, September 2008/December 2008


p=ncdump(f,'-q');  % get info on all variables
switch nargout
    case 0
        for i=1:length(p),
            v=p(i).varname;
            assignin('caller',lower(v),ncread(f,v));
        end
    case 1
    a=[];
    for i=1:length(p),
            v=p(i).varname;
            a=setfield(a,lower(v),ncread(f,v));
    end
    varargout(1)={a};
end

end
