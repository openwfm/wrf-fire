function [long,lat,uf,vf,dzdxf,dzdyf,time_now,bound]=read_data_from_wrfout(wrfout,time)

format long
unit_long=ncread(wrfout,'UNIT_FXLONG');
unit_lat=ncread(wrfout,'UNIT_FXLAT');
uf=ncread2(wrfout,'UF',time);
vf=ncread2(wrfout,'VF',time);
dzdxf=ncread2(wrfout,'DZDXF',time);
dzdyf=ncread2(wrfout,'DZDYF',time);
unit_long=unit_long(1);
unit_lat=unit_lat(1);
long=ncread2(wrfout,'FXLONG',time);
lat=ncread(wrfout,'FXLAT',time);
long=long*unit_long;
lat=lat*unit_lat;

mkdir('../../other/Matlab/perimeter/','data_for_perimeter')

cd ../../other/Matlab/perimeter/data_for_perimeter

fid = fopen('data_LONG.txt', 'w');
dlmwrite('data_LONG.txt', long, 'delimiter', '\t','precision', '%.4f');
fclose(fid);

fid = fopen('data_LAT.txt', 'w');
dlmwrite('data_LAT.txt', lat, 'delimiter', '\t','precision', '%.4f');
fclose(fid);

fid = fopen('data_UF.txt', 'w');
dlmwrite('data_UF.txt', uf, 'delimiter', '\t','precision', '%.4f');
fclose(fid);

fid = fopen('data_VF.txt', 'w');
dlmwrite('data_VF.txt', vf, 'delimiter', '\t','precision', '%.4f');
fclose(fid);

fid = fopen('data_DZDXF.txt', 'w');
dlmwrite('data_DZDXF.txt', dzdxf, 'delimiter', '\t','precision', '%.4f');
fclose(fid);

fid = fopen('data_DZDYF.txt', 'w');
dlmwrite('data_DZDYF.txt', dzdyf, 'delimiter', '\t','precision', '%.4f');
fclose(fid);

end

function a=ncread(filename,varname,time)
% a=ncread(filename,varname)
% return one variable as matlab array without extra dimensions

% Jan Mandel, September 2008

a=ncextract2(ncdump(filename,varname),time);
end

function v=ncextract(p)
% v=ncextract(p)
% extract v as matlab array from structure returned by ncdump
% for one variable

% Jan Mandel, September 2008

% time needed = 49

v=squeeze(double(p.var_value(:,:,time)));
end
      





