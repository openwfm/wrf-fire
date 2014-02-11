% to create conus.kml:
% download http://firemapper.sc.egov.usda.gov/data_viirs/kml/conus_hist/conus_20120914.kmz
% and gunzip 
% 
% to create w.mat:
% run Adam's simulation, then in Matlab
% f='wrfout_d05_2012-09-15_00:00:00'; 
% t=nc2struct(f,{'Times'},{});  n=size(t.times,2);  w=nc2struct(f,{'TIGN_G','FXLONG','FXLAT','UNIT_FXLAT','UNIT_FXLONG'},{},n);
% save ~/w.mat w    
%
% to create s.mat:
% s=read_wrfout_sel({'wrfout_d05_2012-09-09_00:00:00','wrfout_d05_2012-09-12_00:00:00','wrfout_d05_2012-09-15_00:00:00'},{'FGRNHFX'}); 
% save ~/s.mat s 

% ****** REQUIRES Matlab 2013a - will not run in earlier versions *******

v=read_fire_kml('conus_viirs.kml');

% v=read_fire_kml('conus_modis.kml');
load w
load s
for i=1:size(s.times,2),d=char(s.times(:,i))';s.tim(i)=datenum(d);end

% establish boundaries

%min_lat=48+4/60+1/360
%max_lat= 48+10/60+51/3600
%min_lon= - (119+24/60+50/3600)
%max_lon= - (119+0/60+35/3600)
min_lat = min(w.fxlat(:))
max_lat = max(w.fxlat(:))
min_lon = min(w.fxlong(:))
max_lon = max(w.fxlong(:))

% select fire detection within the domain
bi=find(v.lon > min_lon & v.lon < max_lon & v.lat > min_lat & v.lat < max_lat);
lon=v.lon(bi);
lat=v.lat(bi);
tim=v.tim(bi);
res=v.res(bi);


% rebase time on the largest tign_g = the time of the last frame

last_time=datenum(char(w.times)');
max_tign_g=max(w.tign_g(:));

tim = tim - last_time;
tign = (w.tign_g - max_tign_g)/(24*60*60);

% plot satellite detection points

figure(1),clf
% plot3(lon,lat,tim,'ko'),
m=500;
n=500;

% interpolate observation time from detection points

%mesh_lon=min_lon+[0:m]*(max_lon-min_lon)/m;
%mesh_lat=min_lat+[0:n]*(max_lat-min_lat)/n;
%[mesh_lon,mesh_lat]=ndgrid(mesh_lon,mesh_lat);

% f=scatteredInterpolant(lon,lat,tim,'natural');

[mm,nn]=size(w.fxlong);
mi=1:ceil((mm+1)/m):mm;
ni=1:ceil((nn+1)/n):nn;
mesh_fxlong=w.fxlong(mi,ni);
mesh_fxlat=w.fxlat(mi,ni);
mesh_fgrnhfx=s.fgrnhfx(mi,ni,:);

mesh_lon = mesh_fxlong;
mesh_lat = mesh_fxlat;

% mesh_tim=f(mesh_lon,mesh_lat);

mesh_tign=tign(mi,ni);
mesh_tign(mesh_tign(:)==max(mesh_tign(:)))=NaN;
surf(mesh_fxlong,mesh_fxlat,mesh_tign,'EdgeAlpha',0,'FaceAlpha',0.5)
grid

% replacing tim by NaN where too far from the detection point

% plot black patches as detection circles

hold on
mesh_tim2 = NaN*zeros(size(mesh_lon));
for i=1:length(bi)
    dist=sqrt(((mesh_lon-lon(i))*w.unit_fxlong).^2 + ((mesh_lat-lat(i))*w.unit_fxlat).^2); % distance in m from measurement i
    mask=dist <= res(i)/2;
    [ix,jx,dummy]=find(mask);
    ii=min(ix):max(ix);
    jj=min(jx):max(jx);
    mesh_tim2(ii,jj)=NaN;
    idx = find(mask);
    mesh_tim2(idx)=tim(i);
    surface(mesh_lon(ii,jj),mesh_lat(ii,jj),mesh_tim2(ii,jj))% mesh_tim2(idx)=mesh_tim(idx);
    if mod(i,10)==0, drawnow,end
end
grid,drawnow

% hold on, surface(mesh_lon,mesh_lat,mesh_tim2),grid on
title('Barker Canyon fire VIIRS fire detection')
zlabel('days')
ylabel('latitude')
xlabel('longitude')

% remove the max time level from the picture and plot simulated times

print_heat_flux=0; % not working well
if print_heat_flux,
stim=datenum(char(s.times'))'-last_time;
maxh=max(mesh_fgrnhfx(:));
caxis([0 maxh*0.2]);
for i=1:size(mesh_fgrnhfx,3)
    %c=mesh_fgrnhfx(:,:,i);
    %t=stim(i)*ones(size(c));
    %surf(mesh_lon,mesh_lat,t,c,'EdgeAlpha',0,'FaceAlpha',0.1)
    %c(c<maxh*1e-5)=NaN;
    %t(c<maxh*1e-5)=NaN;
    %surf(mesh_lon,mesh_lat,t,c,'EdgeAlpha',0,'FaceAlpha',0.8)
    c=s.fgrnhfx(:,:,i);
    t=stim(i)*ones(size(c));
    c(c<maxh*1e-5)=NaN;
    t(c<maxh*1e-5)=NaN;
    surf(w.fxlong,w.fxlat,t,c,'EdgeAlpha',0,'FaceAlpha',0.8)

    drawnow
end
colorbar,grid
end

hold off

