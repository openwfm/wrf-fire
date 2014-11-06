clear
clc
addpath ../netcdf
addpath ../util1_jan

wrfout{1}='wrfout_d01_0001-01-01_00:00:00';
wrfout{2}='wrfout_d01_0001-01-01_00:01:00';
wrfout{3}='wrfout_d01_0001-01-01_00:02:00';
wrfout{4}='wrfout_d01_0001-01-01_00:03:00';
wrfout{5}='';

input_type=1;     % Type of the input, 1- data file; 0 - wrfout file;
input_file='input_data.dat';    % File that contains perimeter data
                  % It is being used only when input_type=1;;  
num_wrf=4;        % The total number of wrfouts that are being used
time_step=6;      % The amount of time_steps in each wrfout
interval=10;      % time step in wrfout in seconds (every 1 min)
time=1;           % the number of the time step in the latest wrfout 
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








