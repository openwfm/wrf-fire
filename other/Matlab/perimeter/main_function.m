data='data_perim.txt';
%data='data_perimeter.txt';

cd data_for_perimeter
addpath ../../perimeter
addpath('../../util1_jan');
addpath('../../netcdf');
addpath('../../vis3d');
%addpath /home/vkondrat/work/wrfout_files

%addpath /data_for_perimeter

[long,lat,uf,vf,dzdxf,dzdyf,time_now,bound]=read_file_perimeter(data,'data_LONG.txt','data_LAT.txt','data_UF.txt','data_VF.txt','data_DZDXF.txt','data_DZDYF.txt',49);
tign=perimeter(long,lat,uf,vf,dzdxf,dzdyf,time_now,bound);





