data='data_perim.txt';
wrfout='wrfout_d05_2012-09-12_21:15:01';
%data='data_perimeter.txt';

cd data_for_perimeter
addpath ../../perimeter
addpath('../../util1_jan');
addpath('../../netcdf');
addpath('../../vis3d');
%addpath /home/vkondrat/work/wrfout_files

%addpath /data_for_perimeter
count=4; % we will be updating the wind every hour;
interval=900; % Every step in wrf-fire is 15min=900sec;

% time =49 for Witch fire;
% time=100; for Baker's fire;
time=100;
[long,lat,uf,vf,dzdxf,dzdyf,time_now,bound]=read_file_perimeter(data,'data_LONG.txt','data_LAT.txt','data_UF.txt','data_VF.txt','data_DZDXF.txt','data_DZDYF.txt',time);
tign=perimeter_in(long,lat,uf,vf,dzdxf,dzdyf,time_now,bound,wrfout,interval,count);





