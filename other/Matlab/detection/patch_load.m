load w

% wind
ss=load('ss');ss=ss.s;
ss.steps=size(ss.times,2);
ss.time=zeros(1,ss.steps);
for i=1:ss.steps,
    ss.time(i)=datenum(char(ss.times(:,i)'));
end
ss.num=1:ss.steps;
ss.min_time=min(ss.time);
ss.max_time=max(ss.time);
% interpolate surface wind to center of the grid
ss.uh=0.5*(ss.uah(1:end-1,:,:)+ss.uah(2:end,:,:));
ss.vh=0.5*(ss.vah(:,1:end-1,:)+ss.vah(:,2:end,:));
load s
load c
fuels

% establish boundaries from simulations

sim.min_lat = min(w.fxlat(:))
sim.max_lat = max(w.fxlat(:));
sim.min_lon = min(w.fxlong(:));
sim.max_lon = max(w.fxlong(:));
sim.min_tign= min(w.tign_g(:));
max_tign= max(w.tign_g(:));
act.x=find(w.tign_g(:)<max_tign);
act.min_lat = min(w.fxlat(act.x));
act.max_lat = max(w.fxlat(act.x));
act.min_lon = min(w.fxlong(act.x));
act.max_lon = max(w.fxlong(act.x));
margin=0.5;
min_lon=max(sim.min_lon,act.min_lon-margin*(act.max_lon-act.min_lon));
min_lat=max(sim.min_lat,act.min_lat-margin*(act.max_lat-act.min_lat));
max_lon=min(sim.max_lon,act.max_lon+margin*(act.max_lon-act.min_lon));
max_lat=min(sim.max_lat,act.max_lat+margin*(act.max_lat-act.min_lat));

default_bounds{1}=[min_lon,max_lon,min_lat,max_lat];
default_bounds{2}=[sim.min_lon,sim.max_lon,sim.min_lat,sim.max_lat];
for i=1:length(default_bounds),fprintf('default bounds %i: %8.5f %8.5f %8.5f %8.5f\n',i,default_bounds{i});end

bounds=input('enter bounds [min_lon,max_lon,min_lat,max_lat] or number of bounds above (1)> ');
if isempty(bounds),bounds=1;end
if length(bounds)==1, bounds=default_bounds{bounds}; end
[ii,jj]=find(w.fxlong>=bounds(1) & w.fxlong<=bounds(2) & w.fxlat >=bounds(3) & w.fxlat <=bounds(4));
ispan=min(ii):max(ii);
jspan=min(jj):max(jj);
if isempty(ispan) | isempty(jspan), error('selection empty'),end

% restrict data for display

red.fxlat=w.fxlat(ispan,jspan);
red.fxlong=w.fxlong(ispan,jspan);
red.tign_g=w.tign_g(ispan,jspan);
red.nfuel_cat=c.nfuel_cat(ispan,jspan);

red.min_lat = min(red.fxlat(:))
red.max_lat = max(red.fxlat(:))
red.min_lon = min(red.fxlong(:))
red.max_lon = max(red.fxlong(:))

red.axis=[red.min_lon,red.max_lon,red.min_lat,red.max_lat];

prefix='TIFs/';
file_search=[prefix,'*.tif.mat'];      % the level2 files processed by geotiff2mat.py

r=readmod14files(file_search,red.axis);

