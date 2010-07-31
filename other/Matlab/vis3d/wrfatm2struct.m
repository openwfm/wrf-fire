function p=wrfatm2struct(filename,timesteps)
% p=wrf_atm_in(filename)

% read WRF NetCDF file atmosphere mesh variables of interest
if ~exist('timesteps','var'),
    timesteps=[];
end
p=nc2struct(filename,{'U','V','W','PH','PHB','HGT','QVAPOR','T','Z0','Times'},{'DX','DY'},timesteps);
a=(p.ph+p.phb)/9.81; % altitude from geopotential, at w-points
p.altitude=(a(:,:,1:end-1,:)+a(:,:,2:end,:))*0.5; % interpolate to center altitude
for i=1:size(p.times,2), % make Times readable
    times{i}=char(p.times(:,1)'); 
end
p.times=times;
end