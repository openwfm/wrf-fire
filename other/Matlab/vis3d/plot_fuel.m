function plot_fuel(f,units)
% plot_fuel(f)
% plot_fuel(f,units)
% arguments
% f     fuel structure, as created by fuels.m
% units 'metric' (default)
%       'Scott-Burgan' (same as 'sb') ch/h and mi/h
% plot rate of spread against wind speed and slope 
% from fuels.m produced by wrf-fire
% example: fuels; plot_fuel(fuel(3))

name=['Fuel model ',f.fuel_name];

if ~exist('units'),
    units='metric';
end

switch units
    case {'SB','sb','Scott-Burgan'}
        wind_unit='20ft (mi/h)';
        ros_unit='ch/h';
        wind_conv=3600/1609.344;
        ros_conv=3600/20.1168;
    otherwise
        wind_unit='6.1m (m/s)';
        ros_unit='m/s';
        wind_conv=1;
        ros_conv=1;
end

figure(1)
plot(f.wind*wind_conv,f.ros_wind*ros_conv)
xlabel(['wind speed at ',wind_unit,')'])
ylabel(['rate of spread (',ros_unit,')'])
title(name)
grid

figure(2)
plot(f.slope,f.ros_slope*ros_conv)
xlabel('slope (1)')
ylabel(['rate of spread (',ros_unit,')'])
name=['Fuel model ',f.fuel_name];
title(name)
grid