clear
clc
addpath /home/vkondrat/projects/wrffire1/wrf-fire/other/Matlab/netcdf
addpath /home/vkondrat/projects/wrffire1/wrf-fire/other/Matlab/util1_jan
addpath ..
addpath Output

wrfout{1}='VK_wrfout_2013-08-11_00_00_00.nc';
wrfout{2}='VK_wrfout_2013-08-11_12_00_00.nc';
wrfout{3}='VK_wrfout_2013-08-13_00_00_00.nc';
wrfout{4}='VK_wrfout_2013-08-12_12_00_00.nc';
wrfout{5}='VK_wrfout_2013-08-13_00_00_00.nc';
wrfout{6}='VK_wrfout_2013-08-13_12_00_00.nc';

input_type=2;     % Type of the input, 1- data file; 0 - wrfout file; 2- fire_area - is given in input_file;
input_file='output_fire_area.txt';
%input_file='Input_data_Patch_Springs_8-12-2013_2123.dat';    
% File that contains perimeter data
                  % It is being used only when input_type=1;;  
num_wrf=6;        % The total number of wrfouts that are being used
time_step=44;      % The amount of time_steps in each wrfout
interval=900;      % time step in wrfout in seconds (every 15 min=900 sec)
time=38;           % the number of the time step in the latest wrfout 
                  %  closest to the perimeter time from below
format long

% read the fire map at perimeter time
[long,lat,fire_area]=read_file_perimeter(wrfout{num_wrf},time,input_type,input_file);
% now have data: long, lat, 
% fire_area - (input_type=0)- burning or not burning (between 0 and 1, 0-1 OK)
%             (input_type=1)- set of ordered points of the boundary 1st=last;
%                  bound(i,1)-horisontal; bound(i,1)-vertical coordinate
tign=perimeter_in(long,lat,fire_area,wrfout,time,interval,time_step,num_wrf);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The information below is not being used for the current problem
%data='data_perim.txt';
%data='';            % contained the coordinates of the fire perimeter from the shapefile
%wrfout='wrfout_d05_2012-09-12_21:15:01';
%wrfout='wrfout_d05_2012-09-09_00:00:00'; - for the earlies Baker's fire
%time=281;
% time =49 for Witch fire;
% time=100; for Baker's fire;








