% set matlab path to include files in wrf/other/Matlab
format compact
d1=[pwd,'/../../../other/Matlab/vis'];
d2=[pwd,'/../../../other/Matlab/util1_jan'];
d3=[pwd,'/../../../other/Matlab/netcdf'];
d4=[pwd,'/../../../../mexnc'];
addpath(d1)
addpath(d2)
addpath(d3)
addpath(d4)
disp(d1)
ls(d1)
disp(d2)
ls(d2)
disp(d3)
ls(d3)
disp(d4)
ls(d4)
