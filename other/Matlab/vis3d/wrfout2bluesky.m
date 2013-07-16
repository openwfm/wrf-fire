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
w=nc2struct(in,{'Times'},{'DX','DY'}); % read netcdf file
t=char(w.times');  % convert times to character matrix one string per row
s=size(t,1);
dx=w.dx;
dy=w.dy;
fprintf('timesteps %i meshstep %g %g\n',s,dx,dy);
w=nc2struct(in,{'XLONG','XLAT','FXLONG','FXLAT'},{},1); % read netcdf file
[m,n]=size(w.xlong);
[fm,fn]=size(w.fxlong);
srx=fm/m;
sry=fn/n;
fdx=dx/srx;
fdy=dy/sry;
fprintf('mesh size %i by %i\n',m,n)
fprintf('fire mesh size %i by %i\n',fm,fn)
fprintf('refinement ratio %g %g\n',srx,sry)
fprintf('fire meshstep %g %g\n',s,fdx,fdy);
lon=w.fxlong(:,:,1);lon=lon(:);
lat=w.fxlat(:,:,1);lat=lat(:);
min_x=min(lon);
max_x=max(lon);
min_y=min(lat);
max_y=max(lat);
lon_ctr=mean(lon); 
lat_ctr=mean(lat);
fprintf('coordinates %g to %g by %g to %g\n',min_x,max_x,min_y,max_y);
fprintf('the center of the domain is at coordinates %g %g\n',lon_ctr,lat_ctr)

lat=w.fxlat(:,:,1);
lon=w.fxlong(:,:,1);
w=nc2struct(in,{'FIRE_AREA'},{}); % read step istep
fprintf('writing file %s\n',out);
fid=fopen(out,'w');
for istep=1:s
    a=w.fire_area(:,:,istep);
    ta=sum(a(:))*fdx*fdy;
    fprintf('timestep %i/%i %s total fire area %g m^2\n',istep,s,t(istep,:),ta)
    if(ta>0),
        cc=bwconncomp(a>0); % find connected components,  image processing toolbox
        for id=1:length(cc.PixelIdxList)
            fire_ctr_lon=mean(lon(cc.PixelIdxList{id}));  % center longitude of the fire area 
            fire_ctr_lat=mean(lat(cc.PixelIdxList{id}));  % center latitude of the fire area 
            fire_acres=fdx*fdy*sum(a(cc.PixelIdxList{id}))/4046.86; % burning acres
            fprintf(fid,'%i, %g, %g, %g, %s\n',id,fire_ctr_lon,fire_ctr_lat,fire_acres,t(istep,:));
            fprintf('%i, %g, %g, %g, %s\n',id,fire_ctr_lon,fire_ctr_lat,fire_acres,t(istep,:));
        end
    end
end
fclose(fid);
end
