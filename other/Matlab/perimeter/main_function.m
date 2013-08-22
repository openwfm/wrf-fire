%data='data_perim.txt';
data='';            % contained the coordinates of the fire perimeter from the shapefile
% Not using data at the moment
wrfout='wrfout_d01_0001-01-01_00:00:00_fuel3';
%wrfout='wrfout_d05_2012-09-12_21:15:01';
%wrfout='wrfout_d05_2012-09-09_00:00:00'; - for the earlies Baker's fire
%time=281;
interval=10; % time step in wrfout in seconds
count=1; % we will be updating the wind every hour 4*15;
% time =49 for Witch fire;
% time=100; for Baker's fire;
time=31;
% since we go back in time
time_now=300.25;


format long

% read burning/not burning man from wrfout, should not set A or tign_g
% can be replaced by any other data acquisition routine

[long,lat,tign_g,A]=read_file_perimeter(wrfout,time);

ros=read_data_from_wrfout(wrfout,time);



% read long, lat from wrfout
% interpolate/resample to the wrf fire grid

% now have data: long, lat, fire_area - burning or not burning (between 0 and 1, 0-1 OK)
% wrfout - has ros

tign=perimeter_in(long,lat,ros,time_now,A,tign_g,wrfout,time,interval,count);





