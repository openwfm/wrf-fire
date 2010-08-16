function p=wrfatm2struct(filename,timesteps)
% p=wrfatm2struct(filename)
% read WRF NetCDF file atmosphere mesh variables of interest as fields in p
% in: 
% filename    string, e.g. 'wrfout_d01_0001-01-01_00:00:00'
% timesteps   vector of integers; use [] for all
% out:
% p           structure with several arrays from WRF and few more 
% additional fields in p computed:
% p.alt_at_w  altitude at w-points
% p.altitude  altitide interpolated to theta-points (centers of 3D cells)
% p.uc,vc,wc  wind interpolated to theta-points (centers of 3D cells)
% p.times     reformatted as as cell array of strings
if ~exist('timesteps','var'),
    timesteps=[];
end
p=nc2struct(filename,{'U','V','W','PH','PHB','HGT','QVAPOR','T','Z0',...
    'XLONG','XLAT','GRNHFX','FGRNHFX','FXLONG','FXLAT','Times'},...
    {'DX','DY'},timesteps);

% add altitude
p.alt_at_w=(p.ph+p.phb)/9.81; % geopotential altitude at w-points
p.altitude=(p.alt_at_w(:,:,1:end-1,:)+p.alt_at_w(:,:,2:end,:))*0.5; % interpolate to center altitude

% add wind
p.uc = 0.5*(p.u(1:end-1,:,:,:) + p.u(2:end,:,:,:));
p.vc = 0.5*(p.v(:,1:end-1,:,:) + p.v(:,2:end,:,:));
p.wc = 0.5*(p.w(:,:,1:end-1,:) + p.w(:,:,2:end,:));


for i=1:size(p.times,2), % make Times readable
    times{i}=char(p.times(:,1)'); 
end
p.times=times;
%test 
max_rel_err_hgt=big(squeeze(p.alt_at_w(:,:,1,:))-squeeze(p.hgt))/big(p.hgt);
fprintf('relative error of geopotential ground altitude %g\n',max_rel_err_hgt)
end