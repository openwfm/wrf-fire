function result=read_data_from_wrfout(wrfout,time)

% for witch its 3100 2600

format long

ncid = netcdf.open(wrfout,'NC_NOWRITE');

varid = netcdf.inqVarID(ncid,char('UNIT_FXLONG'));
unit_long=netcdf.getVar(ncid,varid,time,1);

varid = netcdf.inqVarID(ncid,char('UNIT_FXLAT'));
unit_lat=netcdf.getVar(ncid,varid,time,1);

varid = netcdf.inqVarID(ncid,char('FXLONG'));
long=netcdf.getVar(ncid,varid,[0,0,time],[3000,1800,1]);

varid = netcdf.inqVarID(ncid,char('FXLAT'));
lat=netcdf.getVar(ncid,varid,[0,0,time],[3000,1800,1]);

long=long*unit_long;
lat=lat*unit_lat;

varid = netcdf.inqVarID(ncid,char('UF'));
uf=netcdf.getVar(ncid,varid,[0,0,time],[3000,1800,1]);

varid = netcdf.inqVarID(ncid,char('VF'));
vf=netcdf.getVar(ncid,varid,[0,0,time],[3000,1800,1]);

varid = netcdf.inqVarID(ncid,char('DZDXF'));
dzdxf=netcdf.getVar(ncid,varid,[0,0,time],[3000,1800,1]);

varid = netcdf.inqVarID(ncid,char('DZDYF'));
dzdyf=netcdf.getVar(ncid,varid,[0,0,time],[3000,1800,1]);

mkdir('data_for_perimeter')

cd data_for_perimeter

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
      





