function ncload(f)
% ncload(f)
% load all variables from a netcdf file f
% put their names in lowercase

% Jan Mandel, September 2008

p=ncdump(f,'-q');  % get info on all variables
for i=1:length(p),
    v=p(i).varname;
    assignin('caller',lower(v),ncread(f,v));
end
end
