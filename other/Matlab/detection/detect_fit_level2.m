function p=detect_fit_level2(prefix)

% to create w.mat:

% run Adam's simulation, currently results in
% /share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_200m
% then in Matlab: 
% set up paths by running setup.m in wrf-fire/WRFV3/test/em_fire
% f='wrfout_d01_2013-08-20_00:00:00'; 
% t=nc2struct(f,{'Times'},{});  n=size(t.times,2)  
% w=nc2struct(f,{'Times','TIGN_G','FXLONG','FXLAT','UNIT_FXLAT','UNIT_FXLONG','XLONG','XLAT','NFUEL_CAT'},{'DX','DY'},n);
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

w=load('w');w=w.w;

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

red=subset_domain(w);

disp('Loading and subsetting detections')
    
prefix='TIFs/';
% the level2 file names processed by geotiff2mat.py
p=sort_rsac_files(prefix); 

time_bounds=subset_detection_time(red,p);

g = load_subset_detections(prefix,p,red,time_bounds,fig);
       
[m,n]=size(red.fxlong);
    
% find ignition point
tign=red.tign;
[i_ign,j_ign]=find(tign == min(tign(:)));
if length(i_ign)~=1,error('assuming single ignition point here'),end
    
% set up constraint on ignition point being the same
params.Constr_ign = zeros(m,n); params.Constr_ign(i_ign,j_ign)=1;


% Parameters of the objective function
params.alpha=input_num('penalty coefficient alpha',1/1000);
% TC = W/(900*24); % time constant = fuel gone in one hour
params.TC = 1/24;  % detection time constants in hours
params.stretch=input_num('Tmin,Tmax,Tneg,Tpos',[0.5,10,5,10]);
params.weight=input_num('water,land,low,nominal,high confidence fire',[-1,-1,0.2,0.6,1]);
params.power=input_num('correction smoothness',1.02);
params.doplot=0;
params.dx=444;
params.dy=444;


disp('optimization loop')
h =zeros(m,n); % initial increment
plotstate(3,tign,'Forecast fire arrival time',g);
print('-dpng','tign_forecast.png');

forecast=tign;

fprintf('********** Starting iterations **************\n');


% storage for h maps
maxiter =2;
maxdepth=3;
h_stor = zeros(m,n,maxiter);

for istep=1:maxiter
    
    fprintf('********** Iteration %g/%g **************\n', istep, maxiter);
    
    % initial search direction, normed so that max(abs(search(:))) = 1.0
    [Js,search]=detection_objective(tign,h,g,params); 
    search = -search/big(search); 

    plotstate(5,search,'Search direction',0);
    print('-dpng', sprintf('%s_search_dir_%d.png', prefix, istep));
    [Jsbest,best_stepsize] = linesearch(4.0,Js,tign,h,search,4,maxdepth);
%    plotstate(21,tign+h+3*search,'Line search (magic step_size=3)',detection_time(1));
    fprintf('Iteration %d: best step size %g\n', istep, best_stepsize);
    if(best_stepsize == 0)
        disp('Cannot improve in this search direction anymore, exiting now.');
        break;
    end
    h = h + best_stepsize*search;
    plotstate(10+istep,tign+h,...
        sprintf('Analysis iteration %i [Js=%g]',istep,Jsbest),g);
    print('-dpng',sprintf('%s_descent_iter_%d.png', prefix, istep));
    h_stor(:,:,istep) = h;
end
% rebase the analysis to the original simulation time
analysis=tign+h; 
% w.tign_g = max_sim_time + (24*60*60)*(tign - w_time_datenum)

plotstate(6,analysis,'Analysis',g)

plotstate(7,analysis-forecast,'Analysis - forecast difference',[])

p.red.tign_g = red.max_tign_g + (24*60*60)*(analysis - red.time);

% analysis = max_sim_time + (24*60*60)*(tign+h
p.tign_g=w.tign_g;
p.tign_g(red.ispan,red.jspan)=p.red.tign_g;

% max_sim_time + (24*60*60)*(tign+h + base_time - w_time_datenum);

disp('input the analysis as tign in WRF-SFIRE with fire_perimeter_time=detection time')

    function [Jsmin,best_stepsize] = linesearch(max_step,Js0,tign,h,search,nmesh,max_depth)
        step_low = 0;
        Jslow = Js0;
        step_high = max_step;
        % Jshigh = detection_objective(tign,h+max_step*search);
        for d=1:max_depth
            step_sizes = linspace(step_low,step_high,nmesh+2);
            Jsls = zeros(nmesh+2,1);
            Jsls(1) = Jslow;
            % Jsls(nmesh+2) = Jshigh;
            for i=2:nmesh+2
                Jsls(i) = detection_objective(tign,h+step_sizes(i)*search,g,params);
            end
            Jshigh=Jsls(nmesh+2);
            for i=1:nmesh+2
                str=sprintf('step=%g objective function=%g',step_sizes(i),Jsls(i)); 
                plotstate(20+i,tign+h+step_sizes(i)*search,str,g);
                drawnow
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

    function plotstate(fig,T,s,obs)
        fprintf('Figure %i %s\n',fig,s)
        arg=red;
        arg.tign=T;
        if exist('obs') && isstruct(obs)
            fire_tign3d(fig,arg,red.base_time)
            hold on
            fire_pixels_3d(fig,obs,red.base_time)
            hold off
        else
            fire_tign3d(fig,arg,0)
        end
        title(s)
        drawnow
    end

end % detect_fit_level2

function i=map_index(x,a,b,n)
% find image of x under linear map [a,b] -> [1,m]
% and round to integer
i=round(1+(n-1)*(x-a)/(b-a));
end

function fire_tign3d(fig,red,base_time)
    figure(fig); hold off
    tign=red.tign;
    tol=0.1;
    tign(tign(:)>max(tign(:))-tol)=NaN;
    h=surf(red.fxlong,red.fxlat,tign-base_time); 
    %a=[red.min_lat,red.max_lat,red.min_lon,red.max_lon,-1,red.max_tign/(24*60*60)];
    %axis manual
    %axis(a)
    xlabel('Longitude'),ylabel('Latitude'),zlabel('Days')
    set(h,'EdgeAlpha',0,'FaceAlpha',0.5); % show faces only
    drawnow
end

function fire_pixels_3d(fig,x,base_time)
if length(x)>1,
    for i=1:length(x),
        fire_pixels_3d(fig,x(i),base_time)
    end
    return
end
kk=find(x.data(:)>=7);
if ~isempty(kk),
    rlon=0.5*abs(x.lon(end)-x.lon(1))/(length(x.lon)-1);
    rlat=0.5*abs(x.lat(end)-x.lat(1))/(length(x.lat)-1);
    lon1=x.xlon(kk)-rlon;
    lon2=x.xlon(kk)+rlon;
    lat1=x.xlat(kk)-rlat;
    lat2=x.xlat(kk)+rlat;
    X=[lon1,lon2,lon2,lon1]';
    Y=[lat1,lat1,lat2,lat2]';
    Z=ones(size(X))*(x.time-base_time);
    cmap=cmapmod14;
    C=cmap(x.data(kk)'+1,:);
    C=reshape(C,length(kk),1,3);
    figure(fig);
    patch(X,Y,Z,C);
end
end
