% script plots all detections 

%red = red_ps;
hold on
tign_days = red.tign_g/(24*3600);


plot_state(1,red,'Forecast from cycling',red.tign_g,g_ps,time_bounds(1:2));
hold on
contour3(red.fxlong,red.fxlat,tign_days,40,'g');
%mesh(red.fxlong,red.fxlat,tign_days);
hold off
