function plot_fuel(f)
% plot_fuel(f)
% plot rate of spread against wind speed and slope 
% from fuels.m produced by wrf-fire
% example: fuels; plot_fuel(fuel(3))

name=['Fuel model ',f.fuel_name];

figure(1)
plot(f.wind,f.ros_wind)
xlabel('wind speed (m/s)')
ylabel('rate of spread (m/s)')
title(name)
grid

figure(2)
plot(f.slope,f.ros_slope)
xlabel('slope (1)')
ylabel('rate of spread (m/s)')
name=['Fuel model ',f.fuel_name];
title(name)
grid