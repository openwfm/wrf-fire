% to create conus.kml:
% download http://firemapper.sc.egov.usda.gov/data_viirs/kml/conus_hist/conus_20120914.kmz
% and gunzip 
% 
% to create w.mat:
% run Adam's simulation, then in Matlab
% w=nc2struct('wrfout_d05_2012-09-15_00:00:00',{'TIGN_G','FXLONG','FXLAT','UNIT_FXLAT','UNIT_FXLONG','Times'},{});
% save ~/w.mat w    
%
% to create h.mat:
% h=ncreadandcat({'wrfout_d05_2012-09-09_00:00:00','wrfout_d05_2012-09-12_00:00:00','wrfout_d05_2012-09-15_00:00:00'},{'GRNHFX','Times'}); 
% x=nc2struct('wrfout_d05_2012-09-09_00:00:00',{'XLONG','XLAT'},{},1) 
% save ~/h.mat h x


% ****** REQUIRES Matlab 2013a - will not run in earlier versions *******

v=read_fire_kml('conus_viirs.kml');
load w
load h
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
m=800;
n=800;

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

mesh_lon = mesh_fxlong;
mesh_lat = mesh_fxlat;

% mesh_tim=f(mesh_lon,mesh_lat);

% replacing tim by NaN where too far from the detection point

% plot black patches as detection circles

hold on
mesh_tim2 = NaN*zeros(size(mesh_tim));
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
drawnow

% hold on, surface(mesh_lon,mesh_lat,mesh_tim2),grid on
title('Barker Canyon fire VIIRS fire detection')
zlabel('days')
ylabel('latitude')
xlabel('longitude')

% remove the max time level from the picture and plot simulated times

mesh_tign=tign(mi,ni);
mesh_tign(mesh_tign(:)==max(mesh_tign(:)))=NaN;
surf(mesh_fxlong,mesh_fxlat,mesh_tign,'EdgeAlpha',0,'FaceAlpha',0.1)

h.tim=datenum(char(h.times'))'-last_time;
nsteps = length(h.tim);
planes=6;
maxh=max(h.grnhfx(:));
caxis([0 maxh*0.2]);
for i=[nsteps:-ceil(nsteps/planes):1]
    c=h.grnhfx(:,:,i);
    c(c<maxh*0.00001)=NaN;
    surf(x.xlong,x.xlat,h.tim(i)*ones(size(c)),c,'EdgeAlpha',0,'FaceAlpha',0.8)
    drawnow
end
grid

hold off

