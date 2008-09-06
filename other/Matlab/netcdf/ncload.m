function ncload(f)
% ncload(f)
% load all variables from a netcdf file f

% Jan Mandel, September 2008

p=ndump(f);  % get info on all variables
for i=1:length(p),
    v=p(i).varname;
    asignin('caller',p,ncread(f,p));
end
end
