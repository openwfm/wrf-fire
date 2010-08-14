% run_plot_at_h
% wrfout='fireflux_small/wrfout_d01_2006-02-23_12:43:00';
timestep=5;
levels=[1,2,6,50, 100, 250]; % in m
ilayers=[1,3]; % indices
ilevels=[3 4 5 6];  % indices
alpha=0.2;
p=wrfatm2struct(wrfout,timestep)
p=wind_uv_at_h(p,levels);
plot_wind_at_h(p,ilevels,ilayers,alpha)