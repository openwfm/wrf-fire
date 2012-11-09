data='data_perim.txt';
V=ones(2,12,12);
%V(2,:,:)=0.5*ones(1,12,12);
%t=perimeter(data,V)

addpath ../../other/Matlab/perimeter

[long,lat,time_now,bound]=read_file_perimeter(data,'wrfout_d01_2007-10-21_12:00:00_real_case');
V=ones(2,size(long,1),size(long,2));
B=perimeter(long,lat,time_now,bound,V);
