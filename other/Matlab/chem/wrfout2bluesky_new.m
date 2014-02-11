function wrfout2bluesky(in)
% wrfout2bluesky(in,out)
% extract from wrfout the fire area info for bluesky
% arguments
%   in  wrfout file name, or cell array of file names
%   out bluesky csv file name
% see http://plone.airfire.org/bluesky/framework/comma-separated-value-csv-files 

% Jan Mandel, July/August 2013

if ~exist('in','var')
    default_in={'wrfout_d05_2012-09-09_00:00:00','wrfout_d05_2012-09-12_00:00:00','wrfout_d05_2012-09-15_00:00:00'};
    disp('default wrfout input:')
    for i=1:length(default_in)
        disp(default_in{i})
    end
    in=input('enter wrfout file name: ','s');
    if isempty(in),
        in=default_in;
    end
end
if ~iscell(in),
    in = {in};
end
w=nc2struct(in{1},{'XLONG','XLAT','FXLONG','FXLAT','NFUEL_CAT','FGIP','FIRE_AREA'},...
    {'DX','DY'},1); % get what does not change from the first netcdf file
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
%emission_factors=read_namelist_fire_emissions('namelist.fire_emissions_bluesky') % emissions factors table
%species=fields(emission_factors);
species={};
nspecies=length(species);
lat=w.fxlat(:,:,1);
lon=w.fxlong(:,:,1);
fgip=w.fgip;
nfuel_cat=w.nfuel_cat;


loc=open_out('fire_locations.csv');
hou=open_out('fire_hourly.csv');

% header
header_loc='id,hour,latitude,longitude,date_time,area,heat,type,moisture_1hr,moisture_10hr,moisture_100hr,moisture_1000hr,moisture_live';
fprintf(loc,header_loc);
header_hou='fire_id,hour,ignition_date_time,area_fract,flame_profile,smolder_profile,residual_profile';
fprintf(hou,header_hou);
for i=1:nspecies
    fprintf(hou,',%s',species{i});
end
fprintf(loc,'\r\n');
fprintf(hou,'\r\n');

% initialize loop over frames
if any(w.fire_area(:)), error('must start from no fire state'),end
area_old=zeros(size(w.fire_area));
mass_old=area_old;
fire_id=0;
for ifile=1:length(in),
    fprintf('reading file %s\n',in{ifile});
    w=nc2struct(in{ifile},{'Times'},{}); % read netcdf file
    t=char(w.times');  % convert times to character matrix one string per row
    s=size(t,1);
    fprintf('timesteps %i meshstep %g %g\n',s,dx,dy);
    for istep=1:s
        d=datenum(t(istep,:),'yyyy-mm-dd_HH:MM:SS'); % convert ESMF time string to double
        tim=[datestr(d,'yyyymmddHHMM'),'Z'];         % convert to bluesky time format
        v = datevec(d);
        if(v(5)==0 & v(6) == 0) % whole hour
        % if(v(6) == 0) % whole minute
            w=nc2struct(in{ifile},{'FIRE_AREA','FUEL_FRAC','FMC_GC'},{},istep); % read step istep
            area=w.fire_area*fdx*fdy;               % fire area (m^2)
            if any(area(:)< area_old(:) - fdx*fdy*eps(single(1)))
                warning('fire area can only increase')
            end 
            area = max(area,area_old);
            ta=sum(area(:));
            mass=fgip.*(1-w.fuel_frac)*fdx*fdy;   % fuel mass burned
            fprintf('timestep %i/%i %s total fire area %g m^2 fuel mass burned %g kg\n',...
                istep,s,t(istep,:),ta,sum(m(:)))
            area_diff = area - area_old;
            mass_diff = mass - mass_old;
            if(ta>0),
                 cc=bwconncomp(area>0); % find connected components, requires image processing toolbox
                 % cc.PixelIdxList{1}=[1:length(lon(:))]'; % just take all
                for id=1:length(cc.PixelIdxList)
                    sub=cc.PixelIdxList{id};      % subset index list
                    fire_id = fire_id+1;
                    fire_ctr_lon=mean(lon(sub));  % center longitude of the fire area 
                    fire_ctr_lat=mean(lat(sub));  % center latitude of the fire area 
                    a_diff_acres=sum(area_diff(sub))/4046.86; % newly burning area (acres)
                    % m_diff_tons=sum(mass_diff(sub))/907.185;  % newly burned fuel mass (tons)
                    h_diff_btus=sum(mass_diff(sub))*17.433e+06*4.30e-04; % newly generated heat (BTUs)
                    sub_coarse=map_submesh(sub,[m,n],[fm,fn]);
                    for m_class=1:5
                        fmc=w.fmc_gc(:,:,m_class);
                        m_mean(m_class)=mean(fmc(sub_coarse))*100;
                    end
                    fmt_loc='%i,%g,%08g,%08g,%s,%g,%g,RX,%g,%g,%g,%g,%g';
                    arg_loc={fire_id,0,fire_ctr_lat,fire_ctr_lon,tim,...
                        a_diff_acres,h_diff_btus,m_mean};
                    fprintf(loc,fmt_loc,arg_loc{:});
                    fmt_hou='%i,%i,%s,%g,%g,%g,%g';
                    arg_hou={fire_id,0,tim,1, 0.9, 0.1, 0};
                    fprintf(hou,fmt_hou,arg_hou{:});
                    nfuel_cat_sub=nfuel_cat(sub);
                    for i=1:nspecies
                        factors=emission_factors.(species{i})';
                        emiss_diff=m_diff(sub).*factors(nfuel_cat_sub); % emissions = mass burned * emission factor (fuel category), on the subset
                        emiss_diff_tons = sum(emiss_diff) /  907.185e3; % total emission of this species in tons, in the subset
                        fprintf(hou,',%g',emiss_diff_tons);
                    end
                    fprintf(loc,'\r\n');
                    fprintf(hou,'\r\n');
                end
            end
            a_old=area;
        else
            fprintf('skipping %s\n',t(istep,:))
        end
    end
end
fclose(loc);
fclose(hou);
end

function fid=open_out(out)
fid=fopen(out,'w');
if fid < 0,
    error(['Cannot open output file ',out])
end
end
