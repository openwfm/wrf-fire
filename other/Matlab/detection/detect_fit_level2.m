function p=detect_fit_level2(prefix)

% to create w.mat:

% run Adam's simulation, currently results in
% /share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_200m
% then in Matlab
% arrays needed only once
% f='wrfout_d01_2013-08-20_00:00:00'; 
% t=nc2struct(f,{'Times'},{});  n=size(t.times,2)  
% w=nc2struct(f,{'Times','TIGN_G','FXLONG','FXLAT','UNIT_FXLAT','UNIT_FXLONG','XLONG','XLAT','NFUEL_CAT'},{'DX','DY'},n);
% save ~/w.mat w    

% fuels.m is created by WRF-SFIRE at the beginning of the run
    
% ****** REQUIRES Matlab 2013a - will not run in earlier versions *******

dx=444;
dy=444;

% figures
fig_map=0;
fig_3d=0;
fig_3d=0;
fig_interp=0;

plot_also_wind=0;

timefmt='dd-mmm-yyyy HH:MM:SS';

disp('Loading simulation')

load w

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

disp('Subset space from simulation')

sim.min_lat = min(w.fxlat(:));
sim.max_lat = max(w.fxlat(:));
sim.min_lon = min(w.fxlong(:));
sim.max_lon = max(w.fxlong(:));
sim.min_tign= min(w.tign_g(:));
sim.max_tign= max(w.tign_g(:));
act.x=find(w.tign_g(:)<sim.max_tign);
act.min_lat = min(w.fxlat(act.x));
act.max_lat = max(w.fxlat(act.x));
act.min_lon = min(w.fxlong(act.x));
act.max_lon = max(w.fxlong(act.x));
margin=0.5;
fprintf('enter relative margin around the fire (%g)',margin);
in=input(' > ');
if ~isempty(in),margin=in;end
min_lon=max(sim.min_lon,act.min_lon-margin*(act.max_lon-act.min_lon));
min_lat=max(sim.min_lat,act.min_lat-margin*(act.max_lat-act.min_lat));
max_lon=min(sim.max_lon,act.max_lon+margin*(act.max_lon-act.min_lon));
max_lat=min(sim.max_lat,act.max_lat+margin*(act.max_lat-act.min_lat));

default_bounds{1}=[min_lon,max_lon,min_lat,max_lat];
default_bounds{2}=[sim.min_lon,sim.max_lon,sim.min_lat,sim.max_lat];
for i=1:length(default_bounds),fprintf('default bounds %i: %8.5f %8.5f %8.5f %8.5f\n',i,default_bounds{i});end

bounds=input('enter bounds [min_lon,max_lon,min_lat,max_lat] or number of bounds above (1)> ');
if isempty(bounds),bounds=1;end
if length(bounds)==1,
    bounds=default_bounds{bounds};
end
[ii,jj]=find(w.fxlong>=bounds(1) & w.fxlong<=bounds(2) & w.fxlat >=bounds(3) & w.fxlat <=bounds(4));
ispan=min(ii):max(ii);
jspan=min(jj):max(jj);
if isempty(ispan) | isempty(jspan), error('selection empty'),end

% restrict simulation

red.fxlat=w.fxlat(ispan,jspan);
red.fxlong=w.fxlong(ispan,jspan);
red.tign_g=w.tign_g(ispan,jspan);
red.nfuel_cat=w.nfuel_cat(ispan,jspan);

red.min_lat = min(red.fxlat(:));
red.max_lat = max(red.fxlat(:));
red.min_lon = min(red.fxlong(:));
red.max_lon = max(red.fxlong(:));

% convert tign_g to datenum 
w.time=datenum(char(w.times)');
red.max_tign_g=max(red.tign_g(:));
red.tign=(red.tign_g - red.max_tign_g)/(24*60*60) + w.time;
min_tign=min(red.tign(:));
max_tign=max(red.tign(:));
base_time=min_tign;

disp('Subsetting detection time')
    
prefix='TIFs/';
% the level2 files processed by geotiff2mat.py
p=sort_rsac_files(prefix); 
min_det_time=p.time(1);
max_det_time=p.time(end);
    
function str=stime(t)
    if ~isscalar(t) | ~isnumeric(t),
        error('t must be a number')
    end
    str=sprintf('%s day %g',datestr(t,timefmt),t-base_time);
end
  
function print_time_bounds(str,time1,time2)
    fprintf('%-10s from %s to %s\n',str,stime(time1),stime(time2))
end
  
% choose time bounds
print_time_bounds('Simulation',min_tign,max_tign)
print_time_bounds('Detections',min_det_time,max_det_time)
b1=max(min_tign,min_det_time);
b2=min(max_tign,max_det_time);
ba=0.5*(b1+b2);
bd=b2-b1;
default_time_bounds{1}=[b1,b2];
default_time_bounds{2}=[b1,b1+0.3*bd];
default_time_bounds{3}=[b1,b1+0.5*bd];
default_time_bounds{4}=[ba-0.2*bd,ba+0.2*bd];
for i=1:length(default_time_bounds)
    str=sprintf('bounds %i',i);
    print_time_bounds(str,default_time_bounds{i}(1),default_time_bounds{i}(2)) 
end
time_bounds=input_num('bounds [min_time max_time] or number of bounds above',3);
if length(time_bounds)==1, 
    time_bounds=default_time_bounds{time_bounds};
else
    time_bounds=time_bounds+base_time;
end
min_time=time_bounds(1);
max_time=time_bounds(2);
print_time_bounds('Using bounds',min_time,max_time)

% prepare for plots
cmap=cmapmod14;
cmap2=cmap;
cmap2(1:7,:)=NaN;
plot_all_level2=true;
red.tign_disp=red.tign;
red.tign_disp(find(red.tign==max_tign))=NaN; % squash the top

if fig_3d>0,
    fire_tign3d(fig_3d,red,base_time)
end
if fig_map,
    figure(fig_map);clf
end

% getting the list of active fires detection files
prefix='TIFs/';    % the level2 files processed by geotiff2mat.py
p=sort_rsac_files(prefix);  % file list sorted by time
itime=find(p.time>=min_time & p.time<=max_time);
d=p.file(itime);      % files within the specified time
t=p.time(itime);
fprintf('Selected %i files in the given time bounds, from %i total.\n',...
    length(d),length(p.time))

k=0;
for i=1:length(d),
    file=d{i};
    fprintf('%s file %s ',stime(t(i)),file);
    v=readmod14(prefix,file,'silent');
    % select fire detection within the domain
    xj=find(v.lon > red.min_lon & v.lon < red.max_lon);
    xi=find(v.lat > red.min_lat & v.lat < red.max_lat);
    ax=[red.min_lon red.max_lon red.min_lat red.max_lat];
    if isempty(xi) | isempty(xj)
        fprintf('outside of the domain\n');
    else
        x=[];
        x.data=v.data(xi,xj);    % subset data
        x.det(1)=sum(x.data(:)==3); % water 
        x.det(2)=sum(x.data(:)==5);  % land
        x.det(3)=sum((x.data(:)==7)); % low confidence fire
        x.det(4)=sum((x.data(:)==8)); % medium confidence fire
        x.det(5)=sum((x.data(:)==9)); % high confidence fire
        if ~any(x.det) 
            fprintf(' no data in the domain\n')
        else
            k=k+1;
            fprintf('water %i land %i fire low %i med %i high %i\n',x.det)
            x.axis=[red.min_lon,red.max_lon,red.min_lat,red.max_lat];
            x.file=v.file; 
            x.time=v.time;
            x.lon=v.lon(xj);
            x.lat=v.lat(xi);
            [x.xlon,x.xlat]=meshgrid(x.lon,x.lat);
            x.fxdata=interp2(v.lon,v.lat,v.data,red.fxlong,red.fxlat,'nearest');
            if fig_interp,  % map interpolated data to reduced domain
                figure(fig_interp)
                cmap=cmapmod14;
                c=reshape(cmap(x.fxdata+1,:),[size(x.fxdata),3]);
                surf(red.fxlong,red.fxlat,zeros(size(red.fxlat)),c,'EdgeAlpha',0.2);
                title(['Detection interpolated to fire mesh ',stime(x.time)])
            end
            g(k)=x;   % store the data structure
            if fig_map,
                figure(fig_map);clf
                showmod14(x)
                hold on
                contour(red.fxlong,red.fxlat,red.tign,[v.time v.time],'-k');
                %fprintf('image time            %s\n',datestr(x.time));
                if plot_also_wind, 
                    if x.time >= ss.min_time && x.time <= ss.max_time,
                        step=interp1(ss.time,ss.num,x.time);
                        step0=floor(step);
                        if step0 < ss.steps,
                            step1 = step0+1;
                        else
                            step1=step0;
                            step0=step1-1;
                        end
                        w0=step1-step;
                        w1=step-step0;
                        uu=w0*ss.uh(:,:,step0)+w1*ss.uh(:,:,step1);
                        vv=w0*ss.vh(:,:,step0)+w1*ss.vh(:,:,step1);
                        fprintf('wind interpolated to %s from\n',datestr(x.time))
                        fprintf('step %i %s weight %8.3f\n',step0,datestr(ss.time(step0)),w0)
                        fprintf('step %i %s weight %8.3f\n',step1,datestr(ss.time(step1)),w1)
                        fprintf('wind interpolated to %s from\n',datestr(x.time))
                        sc=0.006;quiver(w.xlong,w.xlat,sc*uu,sc*vv,0);
                    end
                end
                hold off
                axis(ax)
                drawnow
                % M(k)=getframe(gcf);
                % print(fig_map,'-dpng',['fig',v.timestr]);
            end
            if fig_3d,
                hold on; fire_pixels_3d(fig_3d,x,base_time)
            end
        end
    end
end
fprintf('%i detections selected\n',length(g))
       
    [m,n]=size(red.fxlong);
    
    % find ignition point
    tign=red.tign;
    [i_ign,j_ign]=find(tign == min(tign(:)));
    if length(i_ign)~=1,error('assuming single ignition point here'),end
    
    % set up constraint on ignition point being the same
    Constr_ign = zeros(m,n); Constr_ign(i_ign,j_ign)=1;
        
disp('optimization loop')
h =zeros(m,n); % initial increment
plotstate(3,tign,'Forecast fire arrival time',g);
print('-dpng','tign_forecast.png');

forecast=tign;

fprintf('********** Starting iterations **************\n');

% can change the objective function here
alpha=input_num('penalty coefficient alpha',1/1000);
% TC = W/(900*24); % time constant = fuel gone in one hour
TC = 1/24;  % detection time constants in hours
stretch=input_num('Tmin,Tmax,Tneg,Tpos',[0.5,10,5,10]);
weight=input_num('water,land,low,nominal,high confidence fire',[-1,-1,0.2,0.6,1]);
power=input_num('correction smoothness',1.02);

% storage for h maps
maxiter =2;
maxdepth=3;
h_stor = zeros(m,n,maxiter);

for istep=1:maxiter
    
    fprintf('********** Iteration %g/%g **************\n', istep, maxiter);
    
    % initial search direction, normed so that max(abs(search(:))) = 1.0
    [Js,search]=objective(tign,h); 
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

p.red.tign_g = red.max_tign_g + (24*60*60)*(analysis - w.time);

% analysis = max_sim_time + (24*60*60)*(tign+h
p.tign_g=w.tign_g;
p.tign_g(ispan,jspan)=p.red.tign_g;

% max_sim_time + (24*60*60)*(tign+h + base_time - w_time_datenum);

disp('input the analysis as tign in WRF-SFIRE with fire_perimeter_time=detection time')


    function varargout=objective(tign,h,doplot)
        % [J,delta]=objective(tign,h,doplot)
        % J=objective(tign,h,doplot)
        % compute objective function and optionally gradient delta direction
        T=tign+h;
        f0=0;
        f1=0;
        for k=1:length(g)
            psi = ...
                + weight(1)*(g(k).fxdata==3)... 
                + weight(2)*(g(k).fxdata==5)...
                + weight(3)*(g(k).fxdata==7)...
                + weight(4)*(g(k).fxdata==8)...
                + weight(5)*(g(k).fxdata==9); 
            [f0k,f1k]=like2(psi,g(k).time-T,TC*stretch);
            detections=sum(psi(:)>0);
            f0=f0+f0k;
            f1=f1+f1k;
            % figure(14);mesh(red.fxlong,red.fxlat,psi),title('psi')
            % figure(15);mesh(red.fxlong,red.fxlat,f0k),title('likelihood')
            % figure(16);mesh(red.fxlong,red.fxlat,f1k),title('gradient')
            % drawnow
        end
        %figure(15);mesh(red.fxlong,red.fxlat,f0k),title('likelihood')
        %figure(16);mesh(red.fxlong,red.fxlat,f1k),title('gradient')
        drawnow
        F=f1;
        % objective function and preconditioned gradient
        Ah = poisson_fft2(h,[dx,dy],power);
        % compute both parts of the objective function and compare
        J1 = 0.5*(h(:)'*Ah(:));
        J2 = -ssum(f0);
        J = alpha*J1 + J2;
        fprintf('Objective function J=%g (J1=%g, J2=%g)\n',J,J1,J2);
        if nargout==1,
            varargout={J};
            return
        end
        gradJ = alpha*Ah + F;
        fprintf('Gradient: norm Ah %g norm F %g\n', norm(Ah,2), norm(F,2));
        if exist('doplot','var'),
            plotstate(7,f0,'Detection likelihood',0);
            plotstate(8,F,'Detection likelihood derivative',0);
            plotstate(10,gradJ,'gradient of J',0);
        end
        delta = solve_saddle(Constr_ign,h,F,@(u) poisson_fft2(u,[dx,dy],-power)/alpha);
        varargout=[{J},{delta}];
        % figure(17);mesh(red.fxlong,red.fxlat,delta),title('delta')
        % plotstate(11,delta,'Preconditioned gradient',0);
        %fprintf('norm(grad(J))=%g norm(delta)=%g\n',norm(gradJ,'fro'),norm(delta,'fro'))
    end

    function [Jsmin,best_stepsize] = linesearch(max_step,Js0,tign,h,search,nmesh,max_depth)
        step_low = 0;
        Jslow = Js0;
        step_high = max_step;
        % Jshigh = objective(tign,h+max_step*search);
        for d=1:max_depth
            step_sizes = linspace(step_low,step_high,nmesh+2);
            Jsls = zeros(nmesh+2,1);
            Jsls(1) = Jslow;
            % Jsls(nmesh+2) = Jshigh;
            for i=2:nmesh+2
                Jsls(i) = objective(tign,h+step_sizes(i)*search);
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
            fire_tign3d(fig,arg,base_time)
            hold on
            fire_pixels_3d(fig,obs,base_time)
            hold off
        else
            fire_tign3d(fig,arg,0)
        end
        title(s)
        drawnow
    end

end % detect_fit

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

