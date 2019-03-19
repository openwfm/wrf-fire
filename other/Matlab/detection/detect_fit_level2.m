function a=detect_fit_level2(varargin)
% a=detect_fit_level2
% a=detect_fit_level2(cycle,time_bounds,disp_bounds,w,force)
% arguments
%   cycle       cycle number, for display and output file names
%   time_bounds [observation start, end, spinup start, end] (datenum)
%   disp_bounds [longitude min, max, latitude min,max] (degrees)
%   w           the data structure if not to read from w.mat
%   force       use defaults

% arguments
cycle=[];       if nargin>=1,cycle=varargin{1};end
time_bounds=[]; if nargin>=2,time_bounds=varargin{2};end
disp_bounds=[]; if nargin>=3,disp_bounds=varargin{3};end
w=[];           if nargin>=4,w=varargin{4};end
force=0;;       if nargin>=5,force=varargin{5};end
if nargin>5, error('too many arguments'),end

% to create w.mat:

% run Adam's simulation, currently results in
% /share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_200m
% then in Matlab: 
% set up paths by running setup.m in wrf-fire/WRFV3/test/em_fire
% f='wrfout_d01_2013-08-20_00:00:00'; 
% w=read_wrfout_tign(f);
% save ~/w.mat w    
% fuels.m is created by WRF-SFIRE at the beginning of the run
% copy w.mat and fuel.m to your machine where this fuction will run
% start matlab, set up paths by running setup.m in wrf-fire/WRFV3/test/em_fire
% converge active fires detection geotiff to matlab by 
% wrf-fire/other/Matlab/detection/geotiff2mat.py	

    
% ****** REQUIRES Matlab 2016a - will not run in earlier versions *******

% figures
fig.fig_map=0;
fig.fig_3d=0;
fig.fig_interp=0;

plot_also_wind=0;

disp('Loading simulation')

if isempty(w),
    w=load('w');w=w.w;
end

if plot_also_wind,
    % wind
    ss=load('ss');ss=ss.s;
    ss.steps=size(ss.times,2);
    ss.time=zeros(1,ss.steps);
    for i=1:ss.steps,
        ss.time(i)=datenum(char(ss.times(:,i)'));
    end
    ss.num=1:ss.steps;
    ss.min_time=min(ss.time);
    ss.max_time=max(ss.time);
    % interpolate surface wind to center of the grid
    ss.uh=0.5*(ss.uah(1:end-1,:,:)+ss.uah(2:end,:,:));
    ss.vh=0.5*(ss.vah(:,1:end-1,:)+ss.vah(:,2:end,:));
    fprintf('min wind field time   %s\n',datestr(ss.min_time));
    fprintf('max wind field time   %s\n',datestr(ss.max_time));

end

fuel=[]; fuels

disp('Subset simulation domain and convert time')

red=subset_domain(w,force);
if ~isempty(disp_bounds)
    red.disp_bounds=disp_bounds;
else
    red.disp_bounds=[red.min_lon,red.max_lon,red.min_lat,red.max_lat];
end
fprintf('display bounds %g %g %g %g\n',red.disp_bounds);

disp('Loading and subsetting detections')
    
prefix='../TIFs/';
% the level2 file names processed by geotiff2mat.py
p=sort_rsac_files(prefix); 

if isempty(time_bounds),
    time_bounds=subset_detection_time(red,p);
end
print_time_bounds(red,'Simulation',red.start_datenum,red.end_datenum)
print_time_bounds(red,'Detections',time_bounds(1),time_bounds(2))
print_time_bounds(red,'Spinup    ',time_bounds(3),time_bounds(4))
red.time_bounds=time_bounds;

g = load_subset_detections(prefix,p,red,time_bounds,fig);
       
[m,n]=size(red.fxlong);
    
% find ignition point
tign=red.tign;
[i_ign,j_ign]=find(tign == min(tign(:)));
if length(i_ign)~=1,error('assuming single ignition point here'),end
    
% set up constraint on ignition point being the same
params.Constr_ign = zeros(m,n); params.Constr_ign(i_ign,j_ign)=1;


% Parameters of the objective function
params.alpha=input_num('penalty coefficient alpha',1/1000,force);
% TC = W/(900*24); % time constant = fuel gone in one hour
params.TC = 1/24;  % detection time constants in hours
params.stretch=input_num('Tmin,Tmax,Tneg,Tpos',[0.5,10,5,10],force);
params.weight=input_num('water,land,low,nominal,high confidence fire',[-1,-1,0.2,0.6,1],force);
params.power=input_num('correction smoothness',1.02,force);
params.doplot=0;
params.dx=444;
params.dy=444;

forecast=tign;

disp('optimization loop')
h =zeros(m,n); % initial increment
plot_state(3,red,'Forecast',forecast,g,time_bounds(1:2));
savefig('forecast',cycle)
plot_state_2d(4,red,'Forecast',forecast,g,time_bounds(2));
savefig('forecast2d',cycle)


fprintf('********** Starting iterations **************\n');


% storage for h maps
maxiter =2;
maxdepth=3;
h_stor = zeros(m,n,maxiter);

for istep=1:maxiter
    
    fprintf('********** Iteration %g/%g **************\n', istep, maxiter);
    
    % initial search direction, normed so that max(abs(search(:))) = 1.0
    [Js,search]=detection_objective(tign,h,g,params,red); 
    search = -search/big(search); 

    print('-dpng', sprintf('%s_search_dir_%d.png', prefix, istep));
    [Jsbest,best_stepsize] = linesearch(4.0,Js,tign,h,search,4,maxdepth);
    plot_state(21,red,'Line search',tign+h+3*search,g,time_bounds(1:3));
    fprintf('Iteration %d: best step size %g\n', istep, best_stepsize);
    if(best_stepsize == 0)
        disp('Cannot improve in this search direction anymore, exiting now.');
        break;
    end
    h = h + best_stepsize*search;
    plot_state(10+istep,red,sprintf('Analysis iteration %i [Js=%g]',istep,Jsbest),tign+h,g,time_bounds(1:2));
    print('-dpng',sprintf('%s_descent_iter_%d.png', prefix, istep));
    h_stor(:,:,istep) = h;
end

analysis=tign+h;
% w.tign_g = max_sim_time + (24*60*60)*(tign - w_time_datenum)

plot_state(6,red,'Analysis',analysis,g,time_bounds(1:2))
savefig('analysis',cycle)
plot_state_2d(5,red,'Analysis',analysis,g,time_bounds(2));
savefig('analysis2d',cycle)
plot_state_2d(7,red,{'Forecast','Analysis'},{forecast,analysis},g,time_bounds(2));
savefig('forecast_analysis2d',cycle)

% spinup - combine analysis and forecast in the strip between
% forecast fire area at restart time and outside of analysis fire area at perimeter time

restart_time=time_bounds(3);
perimeter_time=time_bounds(4);
wf = max(forecast - restart_time,0); % 0 in forecast fire area at restart time, >0 outside 
wa = max(perimeter_time-analysis,0); % 0 outside of analysis fire area at perimeter time, >0 inside

% check if we have inclusion so the strip exist 
shrink=nnz(wa + wf==0);  
if shrink,
    fprintf('Cannot spin up, fire area shrinking in analysis at %g points\n',shrink)
    error('Try an earlier restart time');
end

% map the weights so that 0 ->1, 0->1, 
vf=1-wf./(wf+wa);  % 1  in forecast fire area at restart time, ADDING UP TO 1 
va=1-wa./(wf+wa); %  1  outside of analysis fire area at restart time, ADDING UP TO 1

% combine the forecast and analysis
spinup = vf.*forecast + va.*analysis; 

plot_state(8,red,'Spinup',spinup,g,time_bounds(3:4))
savefig('spinup',cycle)
plot_state(9,red,'Forecast for spinup',forecast,g,time_bounds(3:4))
plot_state(10,red,'Analysis for spinup',analysis,g,time_bounds(3:4))


% convert all time from datenum to seconds since run and insert in full
% field

% insert analysis on reduced domain to the whole thing
a.forecast=w.tign_g;
a.analysis=w.tign_g;
a.analysis(red.ispan,red.jspan)=datenum2time(analysis,red);
a.spinup=w.tign_g;
a.spinup(red.ispan,red.jspan)=datenum2time(spinup,red);

% display bounds
a.disp_bounds=red.disp_bounds;

% copy time bounds to output structure

% as datenum - native
a.observations_start_datenum=time_bounds(1);
a.observations_end_datenum=time_bounds(2);
a.restart_datenum=time_bounds(3);
a.fire_perimeter_datenum=time_bounds(4);

% as seconds since start
a.observations_start_time=datenum2time(time_bounds(1),red);
a.observations_end_time=datenum2time(time_bounds(2),red);
a.restart_time=datenum2time(time_bounds(3),red);
a.fire_perimeter_time=datenum2time(time_bounds(4),red);

% as date strings
a.observations_start_Times=stime(time_bounds(1),red);
a.observations_end_Times=stime(time_bounds(2),red);
a.restart_Times=stime(time_bounds(3),red);
a.fire_perimeter_Times=stime(time_bounds(4),red);

% as days since start
a.observations_start_days=a.observations_start_time/(24*3600);
a.observations_end_days=a.observations_end_time/(24*3600);
a.restart_days=a.restart_time/(24*3600);
a.fire_perimeter_days=a.fire_perimeter_time/(24*3600);

a.cycle=cycle;

fprintf('Input the spinup as TIGN_G and restart\nfrom %s with fire_perimeter_time=%g\n',...
    a.restart_Times,a.fire_perimeter_time)

return

    function [Jsmin,best_stepsize] = linesearch(max_step,Js0,tign,h,search,nmesh,max_depth)
        step_low = 0;
        Jslow = Js0;
        step_high = max_step;
        % Jshigh = detection_objective(tign,h+max_step*search,red);
        for d=1:max_depth
            step_sizes = linspace(step_low,step_high,nmesh+2);
            Jsls = zeros(nmesh+2,1);
            Jsls(1) = Jslow;
            % Jsls(nmesh+2) = Jshigh;
            for i=2:nmesh+2
                Jsls(i) = detection_objective(tign,h+step_sizes(i)*search,g,params,red);
            end
            Jshigh=Jsls(nmesh+2);
            for i=1:nmesh+2
                str=sprintf('step=%g objective function=%g',step_sizes(i),Jsls(i)); 
                plot_state(20+i,red,str,tign+h+step_sizes(i)*search,g,time_bounds(1:2));
            end
            
            figure(8);
            plot(step_sizes,Jsls,'+-');
            title(sprintf('Objective function Js vs. step size, iter=%d,depth=%d',istep,d), 'fontsize', 16);
            xlabel('step\_size [-]');
            ylabel('Js [-]');
            % print('-dpng',sprintf('%s_linesearch_iter_%d_depth_%d.png',prefix,istep,d));
            
            [Jsmin,ndx] = min(Jsls);
            
            low = max(ndx-1,1);
            high = min(ndx+1,nmesh+2);
            Jslow = Jsls(low);
            Jshigh = Jsls(high);
            step_low = step_sizes(low);
            step_high = step_sizes(high);
            if high<nmesh+2,
                step_high = step_sizes(high);
            else
                step_high = step_sizes(high)*2;
            end
        end
                
        best_stepsize = step_sizes(ndx);
    end


end % detect_fit_level2

function i=map_index(x,a,b,n)
% find image of x under linear map [a,b] -> [1,m]
% and round to integer
i=round(1+(n-1)*(x-a)/(b-a));
end

function savefig(file,cycle)
    if isempty(cycle),
        filename=file;
    else
        filename=sprintf('%s_%i',file,cycle);
    end
    h=gcf;
    fprintf('Saving figure %i as %s\n',h.Number,filename)
    print('-dpng',[filename,'.png'])
    saveas(gcf,[filename,'.fig'],'fig')
end
