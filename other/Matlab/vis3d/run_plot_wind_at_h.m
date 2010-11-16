function p=run_plot_wind_at_h(wrfout,timestep,heights,levels,alpha,wscale,hscale)
% p=run_plot_wind_at_h(wrfout,levels,ilayers,alpha)
% 
% in:
% wrfout        file name
% timestep      number of time step to extract out of the wrfout file
% heights       array of heights (m) above the terrain
% levels        indices of discretization levels
% alpha         transparency, between 0 and 1
% wscale        max wind speed for color scale (m/s)
% hscale        max height for z-axis scale (m)
%
% out:
% p          structure with the arrays displayed, and more
%
% examples:
%
% run_plot_wind_at_h('wrfout_d01_2006-02-23_12:43:00',35,6.1,[],1.0,10.0,10)
% run_plot_wind_at_h('wrfout_d01_2006-02-23_12:43:00',35,[6.1],[1:4],0.5,10.0,10)
% for i=1:35,run_plot_wind_at_h('wrfout_d01_2006-02-23_12:43:00',i,[1,6.1],[],1,10.0,10),end
% for i=1:60,run_plot_wind_at_h('wrfout_d01_0001-01-01_00:00:00',i,[1,6.1],[],1,10.0,210),end

% run_plot_at_h
% wrfout='fireflux_small/wrfout_d01_2006-02-23_12:43:00';
%levels=[0.5,2,6,46, 100, 250]; % in m, what gets computed
%ilevels=[]; % indices, which levels from above get displayed
%ilevels=[1,2,3];
%ilayers=[]; % indices of mesh layers to display
%alpha=1;

p=wrfatm2struct(wrfout,timestep)
p=wind_uv_at_h(p,heights);
plot_wind_at_h(p,1:length(heights),levels,alpha,1,wscale,hscale)