%data='data_perim.txt';
data='';
wrfout='wrfout_d01_0001-01-01_00:00:00';
%wrfout='wrfout_d05_2012-09-12_21:15:01';
%wrfout='wrfout_d05_2012-09-09_00:00:00'; - for the earlies Baker's fire
%time=281;
interval=10; % Every step in wrf-fire is 15min=900sec;
count=1; % we will be updating the wind every hour 4*15;
% time =49 for Witch fire;
% time=100; for Baker's fire;
time=31;
time_now=300.25;



[long,lat,ros,A,tign_g]=read_file_perimeter(data,wrfout,time);
tign=perimeter_in(long,lat,ros,time_now,A,tign_g,wrfout,interval,count);





