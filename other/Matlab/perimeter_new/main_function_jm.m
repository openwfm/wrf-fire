%clear
clc
%addpath ../netcdf
%addpath ../util1_jan
%addpath ..
%addpath Output
disp('If you get missing netcdf functions please run startup.m in this directory, other/Matlab/perimeter_new.')
disp('Note: you need to cd to the directory first and then run startup.')
if ~exist('saved_data','var'),
     global saved_data
     saved_data=0;
end


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
num_wrf=3;        % The total number of wrfouts that are being used
frames=48;      % The amount of frames in each wrfout
frame_interval=1800;  % frame interval in wrfout in seconds (every 15 min=900 sec)
last_frame=27;           % the number of the frame in the latest wrfout 
last_frame_time=(frames*(num_wrf-1)+last_frame)*frame_interval

% read the fire map at perimeter time
    [long,lat,fire_perimeter,timestep_end]=read_file_perimeter_jm(wrfout{num_wrf},wrfout{num_wrf}, last_frame,input_type,input_file);

% now have data: long, lat, 
% fire_area - (input_type=0)- burning or not burning (between 0 and 1, 0-1 OK)
%             (input_type=1)- 2set of ordered points of the boundary 1st=last;
%                  bound(i,1)-horisontal; bound(i,1)-vertical coordinate
tign=perimeter_in_jm(long,lat,fire_perimeter,wrfout,last_frame,frame_interval,frames,num_wrf, input_type);
figure(3);mesh(long,lat,tign)
xlabel('long (m)')
ylabel('lat (m)')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The information below is not being used for the current problem
%data='data_perim.txt';
%data='';            % contained the coordinates of the fire perimeter from the shapefile
%wrfout='wrfout_d05_2012-09-12_21:15:01';
%wrfout='wrfout_d05_2012-09-09_00:00:00'; - for the earlies Baker's fire
%time=281;
% time =49 for Witch fire;
% time=100; for Baker's fire;







