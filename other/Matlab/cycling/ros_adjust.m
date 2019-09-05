function adjustment = ros_adjust(forecast, analysis, time, fuel_map, p, w)
% adjustment = ros_adjust(forecast, analysis, time)
% function returns a rate of spread adjustment factor
% inputs:
%       forecast - tign for forecast
%       analysis - tign for analysis
%       time - time step for which comparison between forecast 
%           and analysis is to be made, probably use observation end time
%           for the cycle in question
%       fuel_map - matrix with fuel types, use w.nfuel_cat
%       p  - struct with all the data about the simulation, eventually
%       (possibly) reove the first three paramters of this function since
%       they are in p anyway...
%       w - contains the computational grid...
% 
% output:
%       adjustment - factor with which to multiply ROS adjustments in 
%           namelist.fire

fuel_type = 14;
adj = ones(fuel_type,1);

threshold = 10; %minimum number of grid cells with given fuel type to 

% use detections to estimate burn area, nomimal and high confidence only,
% g is the struct with detection data created in detect_fit_lev2 with the
% following :
% g = load_subset_detections(prefix,p,red,time_bounds,fig);
% move this inside so need to save data as .mat file is removed

%% first create a list of all detection pixels
load g.mat
%vectors for lon, lat of fire pixels
fire_lon = [];
fire_lat =[];
for k = 1:length(g)
    %fprintf('Reading tif %i \n',k);
    mask = g(k).data >= 7;
    x = g(k).xlon(mask);
    fire_lon = [fire_lon;x(:)];
    y = g(k).xlat(mask);
    fire_lat = [fire_lat;y];
end
%% create and draw polygon around the detection pixels
shrink = 0.5;
fire_perim = boundary(fire_lon,fire_lat,shrink);
x_perim = fire_lon(fire_perim);
y_perim = fire_lat(fire_perim);

figure
hold on
plot(x_perim,y_perim);
scatter(fire_lon,fire_lat);
title('Detections and estimated perimeter');
hold off
%% fit this fire perimiter onto the computational grid

%find data inside of perimeter
x_grid = w.fxlong(:);
y_grid = w.fxlat(:);
[in,on] = inpolygon(x_grid,y_grid,x_perim,y_perim);
fires = logical(in+on);
g_fires = sum(fires(:));
w_fires = w.tign_g <= time;
w_count = sum(w_fires(:));
new_adjust = sqrt(g_fires/w_count);

%% computing new ROS to use by exponetial model where
%% log(burn area) = 0.51x+2.539

adjrw=input_num('current adjrw : ',1);
exp_adj = 0.51*log(g_fires/w_count)+adjrw;



%plotting some results
figure
hold on
contour(w.fxlong,w.fxlat,w.tign_g)
scatter(w.fxlong(fires),w.fxlat(fires),'*')
title('Contours of forecast and estimate of burn are from detections')
hold off

figure
hold on
plot(x_perim,y_perim);
scatter(fire_lon,fire_lat);
contour(w.fxlong,w.fxlat,w.tign_g)
title('Detections and estimated perimeter with contours from forecast');
hold off




%old method
a=analysis<=time;
a_count = sum(a(:));
f=forecast<=time;
f_count = sum(f(:));
adjustment = sqrt(a_count/f_count);



fprintf('Recomended single ROS adjustment factor by old method: %f \n',adjustment);
fprintf('Recomended single ROS adjustment factor by new method: %f \n',new_adjust);
fprintf('Recomended new adjrw by exponential method: %f \n',exp_adj);

%trying adjustments by fuel types.....
for i = 1:fuel_type
     
    %fprintf('compute ROS adjustemnt for fuel type %i\n',i);
    mask = fuel_map == i;
    a_new = a.*mask;
    a_new_count = sum(a_new(:));
    f_new = f.*mask;
    f_new_count = sum(f_new(:));
    if f_new_count >=10
        adj(i) = sqrt(a_new_count/f_new_count);
    end
    %fprintf('ROS adjusment factor for fuel type %i : %f \n\n',i,adj(i));
    
    
end

fprintf('adjr0 = %.2f, %.2f, %.2f, %.2f, %.2f, %.2f, %.2f,\n', ...
    adj(1),adj(2),adj(3),adj(4),adj(5),adj(6),adj(7));
fprintf('        %.2f, %.2f, %.2f, %.2f, %.2f, %.2f, %.2f\n', ...
    adj(8),adj(9),adj(10),adj(11),adj(12),adj(13),adj(14));

fprintf('adjrw = %.2f, %.2f, %.2f, %.2f, %.2f, %.2f, %.2f,\n', ...
    adj(1),adj(2),adj(3),adj(4),adj(5),adj(6),adj(7));
fprintf('        %.2f, %.2f, %.2f, %.2f, %.2f, %.2f, %.2f\n', ...
    adj(8),adj(9),adj(10),adj(11),adj(12),adj(13),adj(14));

fprintf('adjrs = %.2f, %.2f, %.2f, %.2f, %.2f, %.2f, %.2f,\n', ...
    adj(1),adj(2),adj(3),adj(4),adj(5),adj(6),adj(7));
fprintf('        %.2f, %.2f, %.2f, %.2f, %.2f, %.2f, %.2f\n', ...
    adj(8),adj(9),adj(10),adj(11),adj(12),adj(13),adj(14));





