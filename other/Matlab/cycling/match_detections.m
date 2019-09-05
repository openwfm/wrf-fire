function [ score ] = match_detections( wrfout )
%function score = match_detections( input_args )
%Function evaluates a simulation by finding how well it was able to predict
%where satellites detections would indicate a fire was burning
%Inputs:
%  wrfout: wrfout file containing the fire arrivak time variable tign
%Output:
%  score: evaluation of the goodness of the fit

%read the wrfout file
% use which time step?
w = read_wrfout_tign(wrfout);
red = subset_domain(w,1);


%use the wrfout file to find subset of detections

time_bounds(2) = red.max_tign - 2;
%time_bounds(2) = 7.354637292824074e+05
time_bounds(1) = red.min_tign
time_bounds(1) = time_bounds(2)-3;
det_prefix = '../TIFs/';
det_list=sort_rsac_files(det_prefix);
fig.fig_map=0;
fig.fig_3d=0;
fig.fig_interp=0;
g = load_subset_detections(det_prefix,det_list,red,time_bounds,fig);

%find list of detections
for i = 1:length(g)
    if i == 1
        fire_mask = g(i).data >=7;
        lons = g(i).xlon(fire_mask);
        lats = g(i).xlat(fire_mask);
    end
    fire_mask = g(i).data >= 7;
    lon_update = g(i).xlon(fire_mask);
    lat_update = g(i).xlat(fire_mask);
    if sum(fire_mask(:)) > 0
        lons = [lons(:);lon_update(:)];
        lats = [lats(:);lat_update(:)];
    end
    %scatter(lons,lats);
    
end %for i...

%find polygon containing fire perimter from wrf
fire_area = red.tign <= time_bounds(2);
% cells within simulation perimeter
fire_lon = red.fxlong(fire_area);
fire_lat = red.fxlat(fire_area);
decimate = 1;
fire_lon = fire_lon(1:decimate:end);
fire_lat = fire_lat(1:decimate:end);
%scatter(fire_lon,fire_lat,'*','r')
fire_boundary = boundary(fire_lon,fire_lat);
x = fire_lon(fire_boundary);
y = fire_lat(fire_boundary);


%[fire_in, fire_on] = inpolygon(red.fxlong(:),red.fxlat(:),x,y);
[in, on] = inpolygon(lons,lats,x,y);
%perim_lon = fire_lon(fire_on);
%perim_y = fire_lat(fire_on);
%calculate score

score = (sum(in)+sum(on))/numel(lats);
score_str = sprintf('%f percent of detections within perimeter',score*100);
figure
hold on
plot(x,y,'r')
scatter(lons(in),lats(in),'g*')
scatter(lons(~in),lats(~in),'b*')
title({'Satellite Fire Detections and Forecast Perimeter',score_str});
legend({'Forecast perimeter','Detections inside perimeter','Detections outside perimeter'});
hold off

% plots simulation perimeter
% scatter(fire_lon(fire_on),fire_lat(fire_on),'r*')


    



end

