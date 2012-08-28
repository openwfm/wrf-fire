function plot_fuel(f,units,scale)
% plot_fuel(f)
% plot_fuel(f,units)
% plot_fuel(f,units,logscale)
% arguments
% f     fuel structure, as created by fuels.m
% units 'metric' (default)
%       'Scott-Burgan' (same as 'sb') ch/h and mi/h
% scale 'direct' (default)
%       'log'  for loglog plots
% plot rate of spread against wind speed and slope 
% from fuels.m produced by wrf-fire
% example: fuels; plot_fuel(fuel(3))

name=['Fuel model ',f.fuel_name];

disp(f.fuel_name)
fprintf('%s fgi = %g\n',f.fgi_descr,f.fgi)
fprintf('%s fuelmc_g = %g\n',f.fuelmc_g_descr,f.fuelmc_g)
fprintf('%s cmbcnst = %g\n',f.cmbcnst_descr,f.cmbcnst)
fprintf('%s weight = %g\n',f.weight_descr,f.weight)

fprintf('\n*** without for evaporation heat of moisture ***\n')
h = f.cmbcnst/(1+f.fuelmc_g);
fprintf('specific heat contents of fuel (J/kg) %g\n',h) 
hd=f.fgi*h;
fprintf('heat density (J/m^2) %g\n',hd)
Tf=f.weight/0.8514; % fuel burns as exp(-t/Tf)
fprintf('initial slope of mass-loss curve (1/s) %g\n',1/Tf)
fprintf('maximal heat flux density (J/m^2/s) %g\n',hd/Tf)
fprintf('\n*** with correction for evaporation of moisture ***\n')
Lw=2.5e6;
fprintf('latent heat of moisure (J/kg) %g\n',Lw)
hw = (f.cmbcnst - Lw*f.fuelmc_g) /(1+f.fuelmc_g);
fprintf('specific heat contents of fuel (J/kg) %g\n',hw)
fprintf('relative correction due to evaporation %g%%\n',100*(hw/h-1)) 
hd=f.fgi*hw;
fprintf('heat density (J/m^2) %g\n',hd)
Tf=f.weight/0.8514; % fuel burns as exp(-t/Tf)
fprintf('initial slope of mass-loss curve (1/s) %g\n',1/Tf)
fprintf('maximal heat flux density (J/m^2/s) %g\n',hd/Tf)


Tf=f.weight/0.8514; % fuel burns as exp(-t/Tf)


if ~exist('units'),
    units='metric';
end

if ~exist('scale'),
    scale='direct';
end
if isempty(units),
    units='metric';
end

switch scale
    case {'log','LOG','loglog'}
        logscale=1;
    otherwise
        logscale=0;
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



figure(1) % wind dependence
for i=1:length(f.wind),
    ros_wind(i) = fire_ros(f,f.wind(i),0);
end
err_wind=big(ros_wind-f.ros_wind)
if logscale,
    loglog(f.wind*wind_conv,ros_wind*ros_conv,'r',f.wind*wind_conv,f.ros_wind*ros_conv,'b')
else
    plot(f.wind*wind_conv,ros_wind*ros_conv,'r',f.wind*wind_conv,f.ros_wind*ros_conv,'b')
end
xlabel(['wind speed at ',wind_unit,')'])
ylabel(['rate of spread (',ros_unit,')'])
title(name)
grid

figure(2) % slope dependence
for i=1:length(f.slope),
    ros_slope(i) = fire_ros(f,0,f.slope(i));
end
err_slope=big(ros_slope-f.ros_slope)
if logscale
    loglog(f.slope,ros_slope*ros_conv,'r',f.slope,f.ros_slope*ros_conv,'b')
else
    plot(f.slope,ros_slope*ros_conv,'r',f.slope,f.ros_slope*ros_conv,'b')
end
xlabel('slope (1)')
ylabel(['rate of spread (',ros_unit,')'])
name=['Fuel model ',f.fuel_name];
title(name)
grid

figure(3)
if(isfield(f,'fmc_g')),
    fmc_g=f.fmc_g;
else
    nsteps=100;
    fmc_g=f.fuelmce*[0:nsteps-1]/nsteps-2;
end
for i=1:length(fmc_g),
    ros_fmc_g(i) = fire_ros(f,0,0,fmc_g(i));
end
if(isfield(f,'fmc_g')),
    err_fmc_g=big(ros_fmc_g-f.ros_fmc_g)
    plot(fmc_g,ros_fmc_g*ros_conv,'r',f.fmc_g,f.ros_fmc_g*ros_conv,'b',...
        f.fuelmc_g,interp1(fmc_g,ros_fmc_g*ros_conv,f.fuelmc_g),'+k')
else
    plot(fmc_g,ros_fmc_g*ros_conv,'b')
end
xlabel('ground fuel moisture content (1)')
ylabel(['rate of spread (',ros_unit,')'])
name=['Fuel model ',f.fuel_name];
title(name)
grid

fprintf('\nzero wind rate of spread %g %s\n',fire_ros(f,0,0)*ros_conv,ros_unit')


fprintf('\nzero wind rate of spread %g %s\n',fire_ros(f,0,0)*ros_conv,ros_unit')



err=big(check_ros(f));
fprintf('\nmax rel err in rate of spread between WRF-Fire and fire_ros.m %g\n',err)