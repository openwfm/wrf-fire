% set matlab path to include files in wrf/other/Matlab
format compact
d={[pwd,'/../../../other/Matlab/vis'],...
    [pwd,'/../../../other/Matlab/util1_jan'],...
    [pwd,'/../../../other/Matlab/netcdf'],...
    [pwd,'/../../../other/Matlab/vis3d'],...
};
for i=1:length(d),
    s=d{i};
    addpath(s)
    disp(s)
    ls(s)
end
clear d i s
