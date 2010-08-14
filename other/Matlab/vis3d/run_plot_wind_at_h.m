% run_plot_at_h
% wrfout='fireflux_small/wrfout_d01_2006-02-23_12:43:00';
timestep=5;
levels=[1,2,6,46, 100, 250]; % in m, what gets computed
ilevels=[4]; % indices, which levels from above get displayed
ilevels=[];
ilayers=[1]; % indices of mesh layers to display
alpha=0.2;
p=wrfatm2struct(wrfout,timestep)
p=wind_uv_at_h(p,levels);
plot_wind_at_h(p,ilevels,ilayers,alpha)