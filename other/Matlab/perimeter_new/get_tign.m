function y = get_tign(wrfinput,input_file,output_file,time,time_step,interval)
%function generating time of ignition matrix for fire spread reversal
%to be saved as a text file named 'output_file', based on the
%wrinput file: 'wrfinput', primeter input text file: 'input_file',
%and the 'time' defining the number of frame from wrfinput file
%closest to the fire perimeter file provided as 'input_file',
%'time_step' the number of frames in the wrfinput file, 
%'interval' time interval in wrfinput in seconds 3600 = 1 hour
%example of usage:
%get_tign('wrfout_d01_2013-08-13_00:00:00','UT-SLD-HU2S Patch Springs 8-12-2013 2123.txt','tign_08132013_0323.txt',8,48,1800)
clc
addpath ../netcdf
addpath ../util1_jan
addpath ..
%addpath Output

display(strcat('wrfinput  :',{'   '}, wrfinput))
display(strcat('perimeter input file  :',{'   '}, input_file))
display(strcat('output file  :',{'   '}, output_file))

wrfout{1}=wrfinput;
%wrfout{2}='wrfout_d01_2013-08-12_00:00:00';
%wrfout{3}='wrfout_d01_2013-08-13_00:00:00';
%wrfout{4}='wrfout_d01_2013-08-14_00:00:00';
%wrfout{5}='wrfout_d01_2013-08-15_00:00:00';
%wrfout{6}='wrfout_d01_2013-08-16_00:00:00';

input_type=2;     % Type of the input, 1- data file; 0 - wrfout file; 2- fire_area - is given in input_file;
%input_file='perimeter.txt';
%input_file='Input_data_Patch_Springs_8-12-2013_2123.dat';    
% File that contains perimeter data
                  % It is being used only when input_type=1;;  
num_wrf=1;        % The total number of wrfouts that are being used
%time_step=48;      % The amount of time_steps in each wrfout
%interval=1800;      % time step in wrfout in seconds (every 15 min=900 sec)
%time=48;           % the number of the time step in the latest wrfout 
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

cmd_mv=['mv output_tign_test.txt ',output_file]
system(cmd_mv)

end




