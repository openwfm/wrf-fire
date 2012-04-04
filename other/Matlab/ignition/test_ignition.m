% Finds artificial time of ignition history for the fire mesh
% Example: witch fire

addpath ../../other/Matlab/ignition

[unit_long,unit_lat,long,lat,time_now,mesh_size,ign_pnt,bound]=read_file_ignition('data.txt','wrfout_d01_2007-10-21_12:00:00_real_case');

B=ignition(unit_long,unit_lat,long,lat,time_now,mesh_size,ign_pnt,bound);


