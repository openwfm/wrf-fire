addpath ../netcdf
addpath ../util1_jan

wrfout='wrfout_d05_2012-09-09_00:00:00_VK_75_to_97';
% Here the netcdf is shortened for the period from timestep 75 till 97
% So time=23 which is 97-74, otherwise it should be 97
%

interval=900;      % time step in wrfout in seconds (every 20 min)
time=23;           % the number of the time step in wrfout closest to the perimeter time from below
%time_now=300.25;  % the perimeter time (s)

format long

% read the fire map at perimeter time

[long,lat,fire_area]=read_file_perimeter(wrfout,time);

% now have data: long, lat, fire_area - burning or not burning (between 0 and 1, 0-1 OK)

tign=perimeter_in(long,lat,fire_area,wrfout,time,interval);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The information below is not beig used for the current problem
%data='data_perim.txt';
%data='';            % contained the coordinates of the fire perimeter from the shapefile
%wrfout='wrfout_d05_2012-09-12_21:15:01';
%wrfout='wrfout_d05_2012-09-09_00:00:00'; - for the earlies Baker's fire
%time=281;
% time =49 for Witch fire;
% time=100; for Baker's fire;








