function p=fire2struct(filename,timesteps)
% p=wrfatm2struct(filename)
% read WRF NetCDF file atmosphere mesh variables of interest as fields in p
% in: 
% filename    string, e.g. 'wrfout_d01_0001-01-01_00:00:00'
% timesteps   vector of integers; use [] for all
% out:
% p           structure with several arrays from WRF and few more 
if ~exist('timesteps','var'),
    timesteps=[];
end
p=nc2struct(filename,{'UAH','VAH','UF','VF','ROS','LFN','R_0',...
    'XLONG','XLAT','GRNHFX','FGRNHFX','FXLONG','FXLAT','Times'},...
    {},timesteps);
for i=1:size(p.times,2), % make Times readable
    times{i}=char(p.times(:,i)');
end
p.times=times;


