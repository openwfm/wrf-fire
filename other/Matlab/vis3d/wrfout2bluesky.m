function wrfout2bluesky(in,out)
% extract from wrfout the fire area info for bluesky
% arguments
%   in  wrfout file name
%   out bluesky csv file name
%
% Jan Mandel, July 2013

if ~exist('in','var')
    in=input('enter wrfout file name: ','s');
end
if ~exist('out','var')
    out=input('enter bluesky csv file name: ','s');
    if isempty(out),
        out='bluesky.csv'; % default
    end
end
fprintf('reading file %s\n',in);
w=nc2struct(in,{'FXLONG','FXLAT','FIRE_AREA','Times'},{'DX','DY'}); % read netcdf file
min_x=min(w.fxlong(:));
max_x=max(w.fxlong(:));
min_y=min(w.fxlat(:));
max_y=max(w.fxlat(:));
lon_ctr=mean(w.fxlong(:)); 
lat_ctr=mean(w.fxlat(:));
[m,n,s]=size(w.fxlong);
fprintf('fire mesh size %i by %i coordinates %g to %g by %g to %g\n',m,n,min_x,max_x,min_y,max_y);
fprintf('the center of the domain is at coordinates %g %g\n',lon_ctr,lat_ctr)
fprintf('timesteps %i\n',s);
t=char(w.times');  % convert times to character matrix one string per row
lat=w.fxlat(:,:,1);
lon=w.fxlong(:,:,1);
fprintf('writing file %s\n',out);
fid=fopen(out,'w');
for i=1:s
    a=w.fire_area(:,:,i);  % burning fire mesh squares
    cc=bwconncomp(a>0); % find connected components,  image processing toolbox
    for id=1:length(cc.PixelIdxList)
        fire_ctr_lon=mean(lon(cc.PixelIdxList{id}));  % center longitude of the fire area 
        fire_ctr_lat=mean(lat(cc.PixelIdxList{id}));  % center latitude of the fire area 
        fire_acres=w.dx*w.dy*sum(a(cc.PixelIdxList{id}))/4046.86; % burning acres
        fprintf(fid,'%i, %g, %g, %g, %s\n',id,fire_ctr_lon,fire_ctr_lat,fire_acres,t(i,:));
    end
end
fclose(fid);
end
