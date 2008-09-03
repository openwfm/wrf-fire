function a=ncread(filename,varname)
% a=ncread(filename,varname)
% return one variable as matlab array without extra dimensions

% Jan Mandel, September 2008

a=ncextract(ncdump(filename,varname));
end