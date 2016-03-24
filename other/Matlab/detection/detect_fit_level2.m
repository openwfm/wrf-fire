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

% figures
figmap=2;
fig3d=0;
fig3d=1;
plot_also_wind=0;
plot_detections=1;
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
red.tign=(red.tign_g - max(red.tign_g(:)))/(24*60*60) + w.time;
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
time_bounds=input_num('bounds [min_time max_time] or number of bounds above',2);
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

if fig3d>0,
    fire_tign3d(fig3d,red)
end
if plot_detections,
    figure(figmap);clf
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
        x.axis=[red.min_lon,red.max_lon,red.min_lat,red.max_lat];
        x.file=v.file; 
        x.time=v.time;
        x.data=v.data(xi,xj);    % subset data
        x.lon=v.lon(xj);
        x.lat=v.lat(xi);
        [x.xlon,x.xlat]=meshgrid(x.lon,x.lat);
        det(1)=sum(x.data(:)==3 | x.data(:)==5);  % water or land
        det(2)=sum((x.data(:)==7)); % low confidence fire
        det(3)=sum((x.data(:)==8)); % medium confidence fire
        det(4)=sum((x.data(:)==9)); % high confidence fire
        if ~any(det) 
            fprintf(' no data in the domain\n')
        else
            k=k+1;
            fprintf('water/land %i fire low %i med %i high %i\n',det)
            g(k)=x;   % store the data structure
            if plot_detections,
                figure(figmap);clf
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
                % print(figmap,'-dpng',['fig',v.timestr]);
            end
            if fig3d,
                hold on; fire_pixels_3d(fig3d,x)
            end
        end
    end
end

function fire_tign3d(fig,red)
    figure(fig); hold off
    tign=red.tign;
    tign(tign(:)==max(tign(:)))=NaN;
    h=surf(red.fxlong,red.fxlat,tign-base_time); 
    xlabel('Longitude'),ylabel('Latitude'),zlabel('Days')
    set(h,'EdgeAlpha',0,'FaceAlpha',0.5); % show faces only
    drawnow
end

function fire_pixels_3d(fig,x)
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
    C=cmap(x.data(kk)',:);
    C=reshape(C,length(kk),1,3);
    figure(fig);
    patch(X,Y,Z,C);
end
end

fprintf('%i detections selected\n',k)

% preprocess/interpolate the data to the simulation mesh
for i=1:k
    
end
        if any(x.data(:)>7) && fig3d>0,
            figure(fig3d)
            x.C2=cmap2(x.data+1,:); % translate data to RGB colormap, NaN=no detection
            x.C2=reshape(x.C2,[size(x.data),size(cmap2,2)]);
            hold on
            h2=surf(x.xlon,x.xlat,(v.time-min_tign)*ones(size(x.data)),x.C2);
            set(h2,'EdgeAlpha',0,'FaceAlpha',1); % show faces only
            hold off
            drawnow
        end
       
    detection_time=tim_ref;
    detection_datenum=tim_ref+base_time;
    detection_datestr=datestr(tim_ref+base_time);
    fprintf('mean detection time %g days from ignition %s UTC\n',...
        detection_time,detection_datestr);
    fprintf('days from ignition  min %8.5f max %8.5f\n',min(tim)-min_tign,max(tim)-min_tign);
    fprintf('longitue           min %8.5f max %8.5f\n',min(lon),max(lon));
    fprintf('latitude            min %8.5f max %8.5f\n',min(lat),max(lat)); 

    % set up reduced resolution plots
    [m,n]=size(fxlong);
    m_plot=m; n_plot=n;
    
    m1=map_index(display_bounds(1),bounds(1),bounds(2),m);
    m2=map_index(display_bounds(2),bounds(1),bounds(2),m);
    n1=map_index(display_bounds(3),bounds(3),bounds(4),n);
    n2=map_index(display_bounds(4),bounds(3),bounds(4),n);    
    mi=m1:ceil((m2-m1+1)/m_plot):m2; % reduced index vectors
    ni=n1:ceil((n2-n1+1)/n_plot):n2;
    mesh_fxlong=fxlong(mi,ni);
    mesh_fxlat=fxlat(mi,ni);
    [mesh_m,mesh_n]=size(mesh_fxlat);

    % find ignition point
    [i_ign,j_ign]=find(tign == min(tign(:)));
    if length(i_ign)~=1,error('assuming single ignition point here'),end
    
    % set up constraint on ignition point being the same
    Constr_ign = zeros(m,n); Constr_ign(i_ign,j_ign)=1;

    %
    % *** create detection mask for data likelihood ***
    %
    detection_mask=zeros(m,n);
    detection_time=tim_ref*ones(m,n);

    % resolution diameter in longitude/latitude units
    rlon=0.5*res/w.unit_fxlong;
    rlat=0.5*res/w.unit_fxlat;

    lon1=lon-rlon;
    lon2=lon+rlon;
    lat1=lat-rlat;
    lat2=lat+rlat;
    for i=1:length(lon),
        square = fxlong>=lon1(i) & fxlong<=lon2(i) & ...
                 fxlat >=lat1(i) & fxlat <=lat2(i);
        detection_mask(square)=1;
    end
    
    % for display in plotstate
    C=0.5*ones(1,length(res));
    X=[lon1,lon2,lon2,lon1]';
    Y=[lat1,lat1,lat2,lat2]';
%    plotstate(1,detection_mask,['Fire detection at ',detection_datestr],[])
    % add ignition point
%    hold on, plot(w.fxlong(i_ign,j_ign),w.fxlat(i_ign,j_ign),'xw'); hold off
    % legend('first ignition at %g %g',w.fxlong(i_ign,j_ign),w.fxlat(i_ign,j_ign))
    
    W = zeros(m,n);
    for j=1:n, for i=1:m
           W(i,j)=fuel(nfuel_cat(i,j)).weight;
    end,end
 
%    plotstate(2,W,'Fuel weight',[])
        
disp('optimization loop')
h =zeros(m,n); % initial increment
plotstate(3,tign,'Forecast fire arrival time',detection_time(1));
print('-dpng','tign_forecast.png');

forecast=tign;
mesh_tign_detect(4,fxlong,fxlat,forecast,v,'Forecast fire arrival time')

fprintf('********** Starting iterations **************\n');

% can change the objective function here
alpha=input_num('penalty coefficient alpha',1/1000);
if(alpha < 0)
    error('Alpha is not allowed to be negative.')
end

% TC = W/(900*24); % time constant = fuel gone in one hour
TC = 1/24;  % detection time constants in hours
stretch=input_num('Tmin,Tmax,Tneg,Tpos',[0.5,10,5,10]);
nodetw=input_num('no fire detection weight',0.5);
power=input_num('negative laplacian power',1.02);

% storage for h maps
maxiter = 2;
maxdepth=2;
h_stor = zeros(m,n,maxiter);

for istep=1:maxiter
    
    fprintf('********** Iteration %g/%g **************\n', istep, maxiter);
    
    psi = detection_mask - nodetw*(1-detection_mask);

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
    plotstate(10+istep,tign+h,sprintf('Analysis iteration %i [Js=%g]',istep,Jsbest),detection_time(1));
    print('-dpng',sprintf('%s_descent_iter_%d.png', prefix, istep));
    h_stor(:,:,istep) = h;
end
% rebase the analysis to the original simulation time
analysis=tign+h; 
% w.tign_g = max_sim_time + (24*60*60)*(tign - w_time_datenum)

mesh_tign_detect(6,fxlong,fxlat,analysis,v,'Analysis fire arrival time')
mesh_tign_detect(7,fxlong,fxlat,analysis-forecast,[],'Analysis - forecast difference')

[p.red.tign,p.red.tign_datenum] = rebase_time_back(tign+h);
% analysis = max_sim_time + (24*60*60)*(tign+h + base_time - w_time_datenum);
% err=big(p.tign_sim-analysis)
[p.time.sfire,p.time.datenum] = rebase_time_back(detection_bounds);
p.time.datestr=datestr(p.time.datenum);
p.tign_g=w.tign_g;
p.tign_g(ispan,jspan)=p.red.tign;

% max_sim_time + (24*60*60)*(tign+h + base_time - w_time_datenum);

disp('input the analysis as tign in WRF-SFIRE with fire_perimeter_time=detection time')

figure(9);
col = 'rgbck';
fill(X,Y,C,'EdgeAlpha',1,'FaceAlpha',0);
for j=1:maxiter
    contour(mesh_fxlong,mesh_fxlat,tign+h_stor(:,:,j),[detection_time(1),detection_time(1)],['-',col(j)]); hold on
end
hold off
title('Contour changes vs. step');
xlabel('Longitude');
ylabel('Latitude');
print('-dpng',sprintf( '%s_contours.png', prefix));

    function [time_sim,time_datenum]=rebase_time_back(time_in)
        time_datenum = time_in + base_time;
        time_sim = max_sim_time + (24*60*60)*(time_datenum - w_time_datenum);
    end

    function varargout=objective(tign,h,doplot)
        % [J,delta]=objective(tign,h,doplot)
        % J=objective(tign,h,doplot)
        % compute objective function and optionally gradient delta direction
        T=tign+h;
        [f0,f1]=like1(psi,detection_time-T,TC*stretch);
        F = f1;             % forcing
        % objective function and preconditioned gradient
        Ah = poisson_fft2(h,[dx,dy],power);
        % compute both parts of the objective function and compare
        J1 = 0.5*(h(:)'*Ah(:));
        J2 = -ssum(psi.*f0);
        J = alpha*J1 + J2;
        fprintf('Objective function J=%g (J1=%g, J2=%g)\n',J,J1,J2);
        if nargout==1,
            varargout={J};
            return
        end
        gradJ = alpha*Ah + F;
        fprintf('Gradient: norm Ah %g norm F %g\n', norm(Ah,2), norm(F,2));
        if exist('doplot','var'),
            plotstate(7,f0,'Detection likelihood',0.5,'-w');
            plotstate(8,f1,'Detection likelihood derivative',0);
            plotstate(9,F,'Forcing',0); 
            plotstate(10,gradJ,'gradient of J',0);
        end
        delta = solve_saddle(Constr_ign,h,F,@(u) poisson_fft2(u,[dx,dy],-power)/alpha);
        varargout=[{J},{delta}];
        % plotstate(11,delta,'Preconditioned gradient',0);
        %fprintf('norm(grad(J))=%g norm(delta)=%g\n',norm(gradJ,'fro'),norm(delta,'fro'))
    end

    function plotstate(fig,T,s,c,linespec)
        fprintf('Figure %i %s\n',fig,s)
        plotmap(fig,mesh_fxlong,mesh_fxlat,T(mi,ni),' ');
        hold on
        hh=fill(X,Y,C,'EdgeAlpha',1,'FaceAlpha',0);
        if ~exist('c','var') || isempty(c) || isnan(c),
            title(s);
        else
            title(sprintf('%s, contour=%g',s,c(1)))
            if ~exist('linespec','var'),
                linespec='-k';
            end
            contour(mesh_fxlong,mesh_fxlat,T(mi,ni),[c c],linespec)            
        end
        hold off
        ratio=[w.unit_fxlat,w.unit_fxlong];
        xlabel longtitude
        ylabel latitude
        ratio=[ratio/norm(ratio),1];
        daspect(ratio)
        axis tight
        drawnow
    end


    function [Jsmin,best_stepsize] = linesearch(max_step,Js0,tign,h,search,nmesh,max_depth)
        step_low = 0;
        Jslow = Js0;
        step_high = max_step;
        Jshigh = objective(tign,h+max_step*search);
        for d=1:max_depth
            step_sizes = linspace(step_low,step_high,nmesh+2);
            Jsls = zeros(nmesh+2,1);
            Jsls(1) = Jslow;
            Jsls(nmesh+2) = Jshigh;
            for i=2:nmesh+1
                Jsls(i) = objective(tign,h+step_sizes(i)*search);
            end
            
            figure(8);
            plot(step_sizes,Jsls,'+-');
            title(sprintf('Objective function Js vs. step size, iter=%d,depth=%d',istep,d), 'fontsize', 16);
            xlabel('step\_size [-]','fontsize',14);
            ylabel('Js [-]','fontsize',14);
            print('-dpng',sprintf('%s_linesearch_iter_%d_depth_%d.png',prefix,istep,d));
            
            [Jsmin,ndx] = min(Jsls);
            
            low = max(ndx-1,1);
            high = min(ndx+1,nmesh+2);
            Jslow = Jsls(low);
            Jshigh = Jsls(high);
            step_low = step_sizes(low);
            step_high = step_sizes(high);
        end
                
        best_stepsize = step_sizes(ndx);
    end

end % detect_fit

function i=map_index(x,a,b,n)
% find image of x under linear map [a,b] -> [1,m]
% and round to integer
i=round(1+(n-1)*(x-a)/(b-a));
end
