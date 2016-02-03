% to create conus.kml:
% download http://firemapper.sc.egov.usda.gov/data_viirs/kml/conus_hist/conus_20120914.kmz
% and gunzip 
% 
% to create w.mat:
% run Adam's simulation, currently results in
% /home/akochans/NASA_WSU/wrf-fire/WRFV3/test/em_barker_moist/wrfoutputfiles_live_0.25
% then in Matlab
% f='wrfout_d05_2012-09-15_00:00:00'; 
% t=nc2struct(f,{'Times'},{});  n=size(t.times,2);  w=nc2struct(f,{'TIGN_G','FXLONG','FXLAT','UNIT_FXLAT','UNIT_FXLONG'},{},n);
% save ~/w.mat w    
%
% to create c.mat
% c=nc2struct(f,{'NFUEL_CAT'},{},1);
% save ~/c.mat c
%
% to create s.mat:
% a=dir('wrfout_d01*');
% s=read_wrfout_sel({a.name},{'FGRNHFX'}); 
% save ~/s.mat s 
% 
% fuels.m is created by WRF-SFIRE at the beginning of the run

% ****** REQUIRES Matlab 2013a - will not run in earlier versions *******

load w
load s
load c
fuels

% establish boundaries from simulations

min_lat = min(w.fxlat(:))
max_lat = max(w.fxlat(:))
min_lon = min(w.fxlong(:))
max_lon = max(w.fxlong(:))
min_tign= min(w.tign_g(:))

default_bounds{1}=[min_lon,max_lon,min_lat,max_lat];
default_bounds{2}=[-119.5, -119.0, 47.95, 48.15];
for i=1:length(default_bounds),fprintf('default bounds %i: %8.5f %8.5f %8.5f %8.5f\n',i,default_bounds{i});end

bounds=input('enter bounds [min_lon,max_lon,min_lat,max_lat] or number of bounds above (1)> ');
if isempty(bounds),bounds=1;end
if length(bounds)==1, bounds=default_bounds{bounds}; end
[ii,jj]=find(w.fxlong>=bounds(1) & w.fxlong<=bounds(2) & w.fxlat >=bounds(3) & w.fxlat <=bounds(4));
ispan=min(ii):max(ii);
jspan=min(jj):max(jj);

% restrict data
w.fxlat=w.fxlat(ispan,jspan);
w.fxlong=w.fxlong(ispan,jspan);
w.tign_g=w.tign_g(ispan,jspan);
c.nfuel_cat=c.nfuel_cat(ispan,jspan);

min_lat = min(w.fxlat(:))
max_lat = max(w.fxlat(:))
min_lon = min(w.fxlong(:))
max_lon = max(w.fxlong(:))

% rebase tign on the largest tign_g = the time of the last frame, in days
last_time=datenum(char(w.times)'); 
tign=(w.tign_g - max(w.tign_g(:)))/(24*60*60) + last_time;
min_tign=min(tign(:));
max_tign=max(tign(:));

tign(find(tign==max_tign))=NaN; % squash the top
h=surf(w.fxlong,w.fxlat,tign-min_tign); 
xlabel('longitude'),ylabel('latitude'),zlabel('days')
set(h,'EdgeAlpha',0,'FaceAlpha',0.5); % show faces only

file_search='TIFs/NPP*.tif.mat';      % the level2 files processed by geotiff2mat.py
d=dir(file_search);d={d.name};

% select fire detection within the domain and time
bii=(v.lon > min_lon & v.lon < max_lon & v.lat > min_lat & v.lat < max_lat);

tim_in = tim_all(bii);
u_in = unique(tim_in);
fprintf('detection times from first ignition\n')
for i=1:length(u_in)
    fprintf('%8.5f days %s UTC %3i %s detections\n',u_in(i)-min_tign,...
    datestr(u_in(i)+last_time),sum(tim_in==u_in(i)),detection);
end
detection_bounds=input('enter detection bounds as [upper,lower]: ')
bi = bii & detection_bounds(1)  + min_tign <= tim_all ... 
         & tim_all <= detection_bounds(2)  + min_tign;
% now detection selected in time and space
lon=v.lon(bi);
lat=v.lat(bi);
res=v.res(bi);
tim=tim_all(bi); 
tim_ref = mean(tim);

fprintf('%i detections selected\n',sum(bi)),
fprintf('mean detection time %g days from ignition %s UTC\n',...
    tim_ref-min_tign,datestr(tim_ref+last_time));
fprintf('days from ignition  min %8.5f max %8.5f\n',min(tim)-min_tign,max(tim)-min_tign);
fprintf('longitude           min %8.5f max %8.5f\n',min(lon),max(lon));
fprintf('latitude            min %8.5f max %8.5f\n',min(lat),max(lat));

% detection selected in time and space
lon=v.lon(bi);
lat=v.lat(bi);
res=v.res(bi);
tim=tim_all(bi); 

% plot satellite detection points

% plot3(lon,lat,tim,'ko'),
% m=500; n=500;
[m,n]=size(w.fxlong);
for j=1:length(fuel), W(j)=fuel(j).weight;end
for j=length(fuel)+1:max(c.nfuel_cat(:)),W(j)=NaN;end
T = zeros(m,n);
for j=1:n, for i=1:m
        T(i,j)=W(c.nfuel_cat(i,j));
end,end

while 1
tscale = 1000; % multiply fuel.weight by this to get detection time scale 
a=input('enter [tscale dir add1p add1m add2p add2m] (1,rad,s/m,s/m)\n');
% magic match to u(1) and viirs seems [1000 3*pi/4 -0.0002  -0.0001 0 0.00005]
tscale=a(1)
if tscale<=0, break, end
dir=a(2)
add1p=a(3)
add1m=a(4)
add2p=a(5)
add2m=a(6)

v1=[cos(dir),sin(dir)]; % main direction
v2=[cos(dir+pi/2),sin(dir+pi/2)]; % secondary direction

% find ignition point
[i_ign,j_ign]=find(w.tign_g == min(w.tign_g(:)));
    
% vector field (x,y) - (x_ign,y_ign) 
VDx=(w.fxlong-w.fxlong(i_ign,j_ign))*w.unit_fxlong;
VDy=(w.fxlat-w.fxlat(i_ign,j_ign))*w.unit_fxlat;
    
p1 = VDx*v1(1)+VDy*v1(2); % projections on main direction
p2 = VDx*v2(1)+VDy*v2(2); % projections on secondary direction
[theta,rho]=cart2pol(p1,p2); % in polar coordinates, theta between [-pi,pi]
thetas=pi*[-3/2,-1,-1/2,0,1/2,1,3/2];
deltas=[add2p add1m add2m add1p add2p add1m add2m];
delta = interp1(thetas,deltas,theta,'pchip').*rho;

tign_mod = tign + delta;

% probability of detection, assuming selected times are close
pmap = p_map(tscale*T/(24*60*60),tign_mod - tim_ref);

[mm,nn]=size(w.fxlong);
mi=1:ceil(mm/m):mm;
ni=1:ceil(nn/n):nn;
mesh_fxlong=w.fxlong(mi,ni);
mesh_fxlat=w.fxlat(mi,ni);
mesh_fgrnhfx=s.fgrnhfx(mi,ni,:);
mesh_pmap = pmap(mi,ni); 
mesh_lon = mesh_fxlong(mi,ni);
mesh_lat = mesh_fxlat(mi,ni);
mesh_tign= tign(mi,ni); 

% draw the fireline
figure(1)
hold off, clf
%contour(mesh_fxlong,mesh_fxlat,mesh_tign,[tim_ref,tim_ref]);

% add the detection probability map
%hold on
h=pcolor(mesh_fxlong,mesh_fxlat,pmap);
set(h,'EdgeAlpha',0,'FaceAlpha',0.5);
colorbar
cc=colormap; cc(1,:)=1; colormap(cc);

hold on
plot(w.fxlong(i_ign,j_ign),w.fxlat(i_ign,j_ign),'xk')
fprintf('first ignition at %g %g, marked by black x\n',w.fxlong(i_ign,j_ign),w.fxlat(i_ign,j_ign))
drawnow
pause(1)

% plot detection squares

C=0.5*ones(1,length(res));
rlon=0.5*res/w.unit_fxlong;
rlat=0.5*res/w.unit_fxlat;
X=[lon-rlon,lon+rlon,lon+rlon,lon-rlon]';
Y=[lat-rlat,lat-rlat,lat+rlat,lat+rlat]';
hold on
hh=fill(X,Y,C);
hold off

grid,drawnow

% hold on, (mesh_lon,mesh_lat,mesh_tim2),grid on
assim_string='';if any(delta(:)),assim_string='assimilation',end
daspect([w.unit_fxlat,w.unit_fxlong,1])                % axis aspect ratio length to length
title(sprintf('Barker Canyon fire %s %s %s UTC',...
    assim_string, detection, datestr(tim_ref+last_time)))
ylabel('latitude')
xlabel('longitude')
drawnow

% evaluate likelihood
ndetect=length(res);
likelihood=zeros(1,ndetect);
mask=cell(1,ndetect);
for i=1:ndetect
    mask{i}=lon(i)-rlon(i) <= w.fxlong & w.fxlong <= lon(i)+rlon(i) & ...
            lat(i)-rlat(i) <= w.fxlat  & w.fxlat  <= lat(i)+rlat(i);
    likelihood(i)=ssum(pmap(mask{i}))/ssum(mask{i});
end
likelihood,
total_likelihood=sum(likelihood) % should be really a product if they are independeny
                                 % but the sum seems to work little better
end