%clear
clc
addpath ../netcdf
addpath ../util1_jan
addpath ..
%addpath Output

%wrfout{1}='/share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_nofire/wrfout_d01_2013-08-11_00:00:00';
% wrfout{2}='/share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_nofire/wrfout_d01_2013-08-12_00:00:00';
% wrfout{1}='/share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_nofire/wrfout_d01_2013-08-13_00:00:00';
% wrfout{4}='/share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_nofire/wrfout_d01_2013-08-14_00:00:00';
% wrfout{5}='/share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_nofire/wrfout_d01_2013-08-15_00:00:00';
% wrfout{6}='/share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_nofire/wrfout_d01_2013-08-16_00:00:00';
% wrfout{7}='/share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_nofire/wrfout_d01_2013-08-17_00:00:00';
% wrfout{8}='/share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_nofire/wrfout_d01_2013-08-18_00:00:00';
% wrfout{9}='/share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_nofire/wrfout_d01_2013-08-19_00:00:00';

wrfout{1}='wrfout_d01_2013-08-11_00:00:00';
wrfout{2}='wrfout_d01_2013-08-12_00:00:00';
wrfout{3}='wrfout_d01_2013-08-13_00:00:00';
wrfout{4}='wrfout_d01_2013-08-14_00:00:00';
wrfout{5}='wrfout_d01_2013-08-15_00:00:00';
wrfout{6}='wrfout_d01_2013-08-16_00:00:00';
wrfout{7}='wrfout_d01_2013-08-17_00:00:00';
wrfout{8}='wrfout_d01_2013-08-18_00:00:00';
wrfout{9}='wrfout_d01_2013-08-19_00:00:00';


input_type=2;     % Type of the input, 1- data file; 0 - wrfout file; 2- fire_area - is given in input_file;
input_file='perimeter.txt';
%input_file='Input_data_Patch_Springs_8-12-2013_2123.dat';    
% File that contains perimeter data
                  % It is being used only when input_type=1;;  
num_wrf=9;        % The total number of wrfouts that are being used
time_step=48;      % The amount of time_steps in each wrfout
interval=1800;      % time step in wrfout in seconds (every 15 min=900 sec)
time=48;           % the number of the time step in the latest wrfout 
                  %  closest to the perimeter time from below
format long

% read the fire map at perimeter time
[long,lat,fire_area]=read_file_perimeter(wrfout{num_wrf},wrfout{num_wrf}, time,input_type,input_file);
% now have data: long, lat, 
% fire_area - (input_type=0)- burning or not burning (between 0 and 1, 0-1 OK)
%             (input_type=1)- set of ordered points of the boundary 1st=last;
%                  bound(i,1)-horisontal; bound(i,1)-vertical coordinate
tign=perimeter_in(long,lat,fire_area,wrfout,time,interval,time_step,num_wrf, input_type);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The information below is not being used for the current problem
%data='data_perim.txt';
%data='';            % contained the coordinates of the fire perimeter from the shapefile
%wrfout='wrfout_d05_2012-09-12_21:15:01';
%wrfout='wrfout_d05_2012-09-09_00:00:00'; - for the earlies Baker's fire
%time=281;
% time =49 for Witch fire;
% time=100; for Baker's fire;








