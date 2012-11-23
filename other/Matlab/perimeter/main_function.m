data='data_perim.txt';
%V(2,:,:)=0.5*ones(1,12,12);
%t=perimeter(data,V)

addpath ../../other/Matlab/perimeter
addpath('../../other/Matlab/util1_jan');
addpath('../../other/Matlab/netcdf');


[long,lat,time_now,bound]=read_file_perimeter(data,'wrfout_d01_2007-10-21_12:00:00_real_case');
V=ones(2,size(long,1),size(long,2));
result=perimeter(long,lat,time_now,bound,V);


