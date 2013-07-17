data='data_perim.txt';
wrfout='wrfout_d05_2012-09-12_21:15:01';
%wrfout='wrfout_d05_2012-09-09_00:00:00'; - for the earlies Baker's fire
%time=281;
interval=900; % Every step in wrf-fire is 15min=900sec;
count=4; % we will be updating the wind every hour 4*15;
% time =49 for Witch fire;
% time=100; for Baker's fire;
time=100;
time_now=216000;
m=3000; % Size of the mesh
n=1800;


cd data_for_perimeter
addpath ../../perimeter
addpath('../../util1_jan');
addpath('../../netcdf');
addpath('../../vis3d');
%addpath /home/vkondrat/work/wrfout_files

[long,lat,ros,bound]=read_file_perimeter(data,wrfout,m,n,time);
tign=perimeter_in(long,lat,ros,time_now,bound,wrfout,interval,count);





