function wrfout2bluesky(in,out)
% wrfout2bluesky(in,out)
% extract from wrfout the fire area info for bluesky
% arguments
%   in  wrfout file name, or cell array of file names
%   out bluesky csv file name

% Jan Mandel, July 2013

if ~exist('in','var')
    in=input('enter wrfout file name: ','s');
end
if ~iscell(in),
    in = {in};
end
if ~exist('out','var')
    out=input('enter bluesky csv file name: ','s');
    if isempty(out),
        out='bluesky.csv'; % default
    end
end
fprintf('reading file %s\n',in{1});
fprintf('writing file %s\n',out);
w=nc2struct(in{1},{'XLONG','XLAT','FXLONG','FXLAT'},{'DX','DY'},1); % get what does not change from the first netcdf file
dx=w.dx;
dy=w.dy;
[m,n]=size(w.xlong);
[fm,fn]=size(w.fxlong);
srx=fm/m;
sry=fn/n;
fdx=dx/srx;
fdy=dy/sry;
fprintf('mesh size %i by %i\n',m,n)
fprintf('fire mesh size %i by %i\n',fm,fn)
fprintf('refinement ratio %g %g\n',srx,sry)
fprintf('fire meshstep %g %g\n',fdx,fdy);
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
fid=fopen(out,'w');
if fid < 0,
    error(['Cannot open output file ',out])
end
lat=w.fxlat(:,:,1);
lon=w.fxlong(:,:,1);
w=nc2struct(in{1},{'FIRE_AREA'},{},1); % the first fire area
a_old=w.fire_area;
fire_id=0;
for ifile=1:length(in),
    fprintf('reading file %s\n',in{ifile});
    w=nc2struct(in{ifile},{'Times'},{}); % read netcdf file
    t=char(w.times');  % convert times to character matrix one string per row
    s=size(t,1);
    fprintf('timesteps %i meshstep %g %g\n',s,dx,dy);
    for istep=1:s
        w=nc2struct(in{ifile},{'FIRE_AREA'},{},istep); % read step istep
        a=w.fire_area;
        ta=sum(a(:))*fdx*fdy;
        fprintf('timestep %i/%i %s total fire area %g m^2\n',istep,s,t(istep,:),ta)
        d=datenum(t(istep,:),'yyyy-mm-dd_HH:MM:SS'); % convert ESMF time string to double
        tim=[datestr(d,'yyyymmddHHMM'),'Z'];         % convert to bluesky time format
        v = datevec(d);
        if(v(5)==0 & v(6) == 0) % whole hour
            a_diff = a - a_old;
            if any(a_diff(:)< - eps(single(1)))
                warning('fire area can only increase')
                a_diff = max(a_diff,0);
            end 
            if(ta>0),
                cc=bwconncomp(a>0); % find connected components,  image processing toolbox
                for id=1:length(cc.PixelIdxList)
                    fire_id = fire_id+1;
                    fire_ctr_lon=mean(lon(cc.PixelIdxList{id}));  % center longitude of the fire area 
                    fire_ctr_lat=mean(lat(cc.PixelIdxList{id}));  % center latitude of the fire area 
                    fire_acres=fdx*fdy*sum(a_diff(cc.PixelIdxList{id}))/4046.86; % newly burning acres
                    fprintf(fid,'%i, %g, %g, %g, %s\n',fire_id,fire_ctr_lon,fire_ctr_lat,fire_acres,tim);
                    fprintf('%i, %g, %g, %g, %s\n',fire_id,fire_ctr_lon,fire_ctr_lat,fire_acres,tim);
                end
            end
            a_old=a;
        else
            fprintf('not on the hour, skipping %s\n',tim)
        end
    end
end
fclose(fid);
end

