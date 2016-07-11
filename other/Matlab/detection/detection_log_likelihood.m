function J=detection_log_likelihood(prefix,w)
% loglike=detection_data_log_likelihood(prefix,w)
% in
%    prefix to search for files, such as 'TIFs/'
%    w      file with extracted wrfout data, see detect_fit_level2
% 
% out
%    J      log likelhood

% changes %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% lines 32,33,35 : remove input num and take default values

% figures switch
fig.fig_state=1;
fig.fig_map=0;
fig.fig_3d=0;
fig.fig_interp=0;

disp('Subset simulation domain and convert time')

red=subset_domain(w);

disp('Loading and subsetting detections')

p=sort_rsac_files(prefix); 
time_bounds=subset_detection_time(red,p);
red.time_bounds=time_bounds;
g = load_subset_detections(prefix,p,red,time_bounds,fig);

% Parameters of the objective function
% params.alpha=input_num('penalty coefficient alpha',1/1000);
% TC = W/(900*24); % time constant = fuel gone in one hour
params.alpha=0;
params.TC = 1/24;  % detection time constants in hours
%params.stretch=input_num('Tmin,Tmax,Tneg,Tpos',[0.5,10,5,10]);
params.stretch=[0.5,10,5,10];
%params.weight=input_num('water,land,low,nominal,high confidence fire',...
%    [-10,-10,0.2,0.6,1]);
params.weight=[-10,-10,0.2,0.6,1];
%params.power=input_num('correction smoothness',1.02);
params.power=1.02;
params.doplot=0;
params.dx=444;
params.dy=444;

% objective function is penalty minus log likelihood -> min
J=-detection_objective(red.tign,0,g,params,red);

s=sprintf('Data log likelihood %18.11e',J);
plot_state(fig.fig_state,red,s,red.tign,g,time_bounds(1:2));

end
