function red=subset_domain(varargin)
% red=subset_domain(w)
% find rectangular domain around fire with some user guidance
% and convert fire arrival time to datenum
%
% input: w structure with fields
%    fxlat, fxlong - latitude, longitude
%    tign_g        - fire arrival time
%    nfuel_cat     - fuel categories
%    times         - time at simulation end as string
%
% ouput: red structure with fields
%    ispan,jspan   - list i,j indices selected
%    fxlat,fxlong  - coordinates on submesh
%    tign_g        - fire arrival time on submesh
%    min_lon,max_lon,min_lat,max_lat - selected box
%    time          - times as datenum
%    max_tign_g    - max fire arrival time in w, corresponds to times
%    tign          - tign_g as datenum
%    min_tign      - min
%    max_tign      - max 
%    base_time     - base time for displays
%
% Changes made %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% original had red=subset_domain(w,red)
% changed to red=subset_domain(varargin)
% 
% comment out margin
% comment out bounds, take default_bounds{2}
% 

% arguments
w = [];  if nargin >=1, w = varargin{1}; end



sim.min_lat = min(w.fxlat(:));
sim.max_lat = max(w.fxlat(:));
sim.min_lon = min(w.fxlong(:));
sim.max_lon = max(w.fxlong(:));
sim.min_tign= min(w.tign_g(:));
sim.max_tign= max(w.tign_g(:));
act.x=find(w.tign_g(:)<sim.max_tign);
act.min_lat = min(w.fxlat(act.x));
act.max_lat = max(w.fxlat(act.x));
act.min_lon = min(w.fxlong(act.x));
act.max_lon = max(w.fxlong(act.x));
margin=0.5;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%commenting out this input
%fprintf('enter relative margin around the fire (%g)',margin);
%in=input(' > ');
%if ~isempty(in),margin=in;end
min_lon=max(sim.min_lon,act.min_lon-margin*(act.max_lon-act.min_lon));
min_lat=max(sim.min_lat,act.min_lat-margin*(act.max_lat-act.min_lat));
max_lon=min(sim.max_lon,act.max_lon+margin*(act.max_lon-act.min_lon));
max_lat=min(sim.max_lat,act.max_lat+margin*(act.max_lat-act.min_lat));

default_bounds{1}=[min_lon,max_lon,min_lat,max_lat];
default_bounds{2}=[sim.min_lon,sim.max_lon,sim.min_lat,sim.max_lat];
for i=1:length(default_bounds),fprintf('default bounds %i: %8.5f %8.5f %8.5f %8.5f\n',i,default_bounds{i});end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%commenting out input
%bounds=input('enter bounds [min_lon,max_lon,min_lat,max_lat] or number of bounds above (1)> ');
%if isempty(bounds),bounds=1;end
%if length(bounds)==1,
%    bounds=default_bounds{bounds};
%end
%take the second default_bounds
bounds = default_bounds{2};
[ii,jj]=find(w.fxlong>=bounds(1) & w.fxlong<=bounds(2) & w.fxlat >=bounds(3) & w.fxlat <=bounds(4));
ispan=min(ii):max(ii);
jspan=min(jj):max(jj);
if isempty(ispan) | isempty(jspan), error('selection empty'),end

% restrict simulation

red.max_tign=sim.max_tign;
red.ispan=ispan;
red.jspan=jspan;
red.fxlat=w.fxlat(ispan,jspan);
red.fxlong=w.fxlong(ispan,jspan);
red.tign_g=w.tign_g(ispan,jspan);
if isfield(w,'nfuel_cat')
    red.nfuel_cat=w.nfuel_cat(ispan,jspan);

red.min_lat = min(red.fxlat(:));
red.max_lat = max(red.fxlat(:));
red.min_lon = min(red.fxlong(:));
red.max_lon = max(red.fxlong(:));

% convert tign_g to datenum 
red.end_datenum=datenum(char(w.times)'); % this time step end
red.end_time=w.dt*w.itimestep; % time from simulation start in seconds
red.start_time=0;
red.start_datenum=red.end_datenum-red.end_time/(24*3600);
fprintf('simulation start seems to be %s\n',datestr(red.start_datenum,'dd-mmm-yyyy HH:MM:SS'));

red.max_tign_g=max(w.tign_g(:));
%red.tign=(red.tign_g - red.max_tign_g)/(24*60*60) + red.time;
red.tign=time2datenum(red.tign_g,red);  % the tign array, in datenum
red.min_tign=min(red.tign(:));
red.max_tign=max(red.tign(:));
end