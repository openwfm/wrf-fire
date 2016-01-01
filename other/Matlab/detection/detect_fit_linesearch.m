function analysis=detect_fit_linesearch(prefix)
% from a copy of barker2

disp('input data')
    % to create conus.kml:
    % download http://firemapper.sc.egov.usda.gov/data_viirs/kml/conus_hist/2012/conus_20120914.kmz
    % and gunzip 
    % 
    % to create w.mat:
    % run Adam's simulation, currently results in
    % /share_home/akochans/NASA_WSU/wrf-fire/WRFV3/test/em_barker_moist/wrfoutputfiles_live_0.25
    % then in Matlab
    % f='wrfout_d05_2012-09-15_00:00:00'; 
    % t=nc2struct(f,{'Times'},{'DX','DY'});  n=size(t.times,2);  w=nc2struct(f,{'TIGN_G','FXLONG','FXLAT','UNIT_FXLAT','UNIT_FXLONG'},{},n);
    % save ~/w.mat t    
    %
    % to create c.mat
    % c=nc2struct(f,{'NFUEL_CAT'},{},1);
    % save ~/c.mat c
    %
    % to create s.mat:
    % s=read_wrfout_sel({'wrfout_d05_2012-09-09_00:00:00','wrfout_d05_2012-09-12_00:00:00','wrfout_d05_2012-09-15_00:00:00'},{'FGRNHFX'}); 
    % save ~/s.mat s 
    % 
    % fuels.m is created by WRF-SFIRE at the beginning of the run
    
    % ****** REQUIRES Matlab 2013a - will not run in earlier versions *******
    
    
    v=read_fire_kml('conus_viirs.kml');
    detection='VIIRS';        
    
    a=load('w');w=a.w;
    if ~isfield('dx',w),
        w.dx=444.44;
        w.dy=444.44;
        warning('fixing up w for old w.mat file from Barker fire')
    end
    
    a=load('s');s=a.s;
    a=load('c');c=a.c;
    fuel.weight=0; % just to let Matlab know what fuel is going to be at compile time
    fuels


disp('subset and process inputs')
    
    % establish boundaries from simulations
    
    min_lat = min(w.fxlat(:))
    max_lat = max(w.fxlat(:))
    min_lon = min(w.fxlong(:))
    max_lon = max(w.fxlong(:))
    min_tign= min(w.tign_g(:))
    
    default_bounds{1}=[min_lon,max_lon,min_lat,max_lat];
    default_bounds{2}=[-119.5, -119.0, 47.95, 48.15];
    display_bounds=default_bounds{2};
%     for i=1:length(default_bounds),fprintf('default bounds %i: %8.5f %8.5f %8.5f %8.5f\n',i,default_bounds{i});end
    
%    bounds=input_num('bounds [min_lon,max_lon,min_lat,max_lat] or number of bounds above',1);
%    if length(bounds)==1, bounds=default_bounds{bounds}; end
    bounds=default_bounds{2};
    [ii,jj]=find(w.fxlong>=bounds(1) & w.fxlong<=bounds(2) & w.fxlat >=bounds(3) & w.fxlat <=bounds(4));
    ispan=min(ii):max(ii);
    jspan=min(jj):max(jj);
    
    % restrict data
    w.fxlat=w.fxlat(ispan,jspan);
    w.fxlong=w.fxlong(ispan,jspan);
    w.tign_g=w.tign_g(ispan,jspan);
    c.nfuel_cat=c.nfuel_cat(ispan,jspan);
    
    min_lat = min(w.fxlat(:))
    max_lat = max(w.fxlat(:))
    min_lon = min(w.fxlong(:))
    max_lon = max(w.fxlong(:))
    min_lon = display_bounds(1);
    max_lon = display_bounds(2);
    min_lat = display_bounds(3);
    max_lat = display_bounds(4);
    
    min_tign= min(w.tign_g(:))
    
    % rebase time on the largest tign_g = the time of the last frame, in days
    
    last_time=datenum(char(w.times)'); 
    max_tign_g=max(w.tign_g(:));
    
    tim_all = v.tim - last_time;
    tign= (w.tign_g - max_tign_g)/(24*60*60);  % now tign is in days
    min_tign= min(tign(:)); % initial ignition time
    tign_disp=tign;
    tign_disp(tign==0)=NaN;      % for display
    
    % select fire detection within the domain and time
    bii=(v.lon > min_lon & v.lon < max_lon & v.lat > min_lat & v.lat < max_lat);
    
    tim_in = tim_all(bii);
    u_in = unique(tim_in);
    fprintf('detection times from first ignition\n')
    for i=1:length(u_in)
        detection_freq(i)=sum(tim_in==u_in(i));
        fprintf('%8.5f days %s UTC %3i %s detections\n',u_in(i)-min_tign,...
        datestr(u_in(i)+last_time),detection_freq(i),detection);
    end
    [max_freq,i]=max(detection_freq);
    tol=0.01;
%    detection_bounds=input_num('detection bounds as [upper,lower]',...
%        [u_in(i)-min_tign-tol,u_in(i)-min_tign+tol]);
    detection_bounds = [u_in(1)-min_tign-tol,u_in(1)-min_tign+tol];
    bi = bii & detection_bounds(1)  + min_tign <= tim_all ... 
             & tim_all <= detection_bounds(2)  + min_tign;
    % now detection selected in time and space
    lon=v.lon(bi);
    lat=v.lat(bi);
    res=v.res(bi);
    tim=tim_all(bi); 
    tim_ref = mean(tim);
    
    fprintf('%i detections selected\n',sum(bi))
    detection_days_from_ignition=tim_ref-min_tign;
    detection_datestr=datestr(tim_ref+last_time);
    fprintf('mean detection time %g days from ignition %s UTC\n',...
        detection_days_from_ignition,detection_datestr);
    fprintf('days from ignition  min %8.5f max %8.5f\n',min(tim)-min_tign,max(tim)-min_tign);
    fprintf('longitude           min %8.5f max %8.5f\n',min(lon),max(lon));
    fprintf('latitude            min %8.5f max %8.5f\n',min(lat),max(lat));
    
    % detection selected in time and space
    lon=v.lon(bi);
    lat=v.lat(bi);
    res=v.res(bi);
    tim=tim_all(bi); 

    % set up reduced resolution plots
    [m,n]=size(w.fxlong);
    m_plot=m; n_plot=n;
    
    m1=map_index(display_bounds(1),bounds(1),bounds(2),m);
    m2=map_index(display_bounds(2),bounds(1),bounds(2),m);
    n1=map_index(display_bounds(3),bounds(3),bounds(4),n);
    n2=map_index(display_bounds(4),bounds(3),bounds(4),n);    
    mi=m1:ceil((m2-m1+1)/m_plot):m2; % reduced index vectors
    ni=n1:ceil((n2-n1+1)/n_plot):n2;
    mesh_fxlong=w.fxlong(mi,ni);
    mesh_fxlat=w.fxlat(mi,ni);
    [mesh_m,mesh_n]=size(mesh_fxlat);

    % find ignition point
    [i_ign,j_ign]=find(w.tign_g == min(w.tign_g(:)));
    if length(i_ign)~=1,error('assuming single ignition point here'),end
    
    % set up constraint on ignition point being the same
    Constr_ign = zeros(m,n); Constr_ign(i_ign,j_ign)=1;

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
        square = w.fxlong>=lon1(i) & w.fxlong<=lon2(i) & ...
                 w.fxlat >=lat1(i) & w.fxlat <=lat2(i);
        detection_mask(square)=1;
    end
    C=0.5*ones(1,length(res));
    X=[lon-rlon,lon+rlon,lon+rlon,lon-rlon]';
    Y=[lat-rlat,lat-rlat,lat+rlat,lat+rlat]';
%    plotstate(1,detection_mask,['Fire detection at ',detection_datestr],[])
    % add ignition point
%    hold on, plot(w.fxlong(i_ign,j_ign),w.fxlat(i_ign,j_ign),'xw'); hold off
    % legend('first ignition at %g %g',w.fxlong(i_ign,j_ign),w.fxlat(i_ign,j_ign))
    
    fuelweight(length(fuel)+1:max(c.nfuel_cat(:)))=NaN;
    for j=1:length(fuel), 
        fuelweight(j)=fuel(j).weight;
    end
    W = zeros(m,n);
    for j=1:n, for i=1:m
           W(i,j)=fuelweight(c.nfuel_cat(i,j));
    end,end
 
%    plotstate(2,W,'Fuel weight',[])
        
disp('optimization loop')
h =zeros(m,n); % initial increment
plotstate(3,tign,'Forecast fire arrival time',detection_time(1));
print('-dpng','tign_forecast.png');


fprintf('********** Starting iterations **************\n');

% can change the objective function here
alpha=input_num('penalty coefficient alpha',1000);
if(alpha < 0)
    error('Alpha is not allowed to be negative.')
end

% TC = W/(900*24); % time constant = fuel gone in one hour
TC = 1/24;  % detection time constants in hours
stretch=input_num('Tmin,Tmax,Tneg,Tpos',[0.5,10,5,10]);
nodetw=input_num('no fire detection weight',0.5);
power=input_num('negative laplacian power',1.02);

% storage for h maps
maxiter = 5;
h_stor = zeros(m,n,maxiter);

for istep=1:maxiter
    
    fprintf('********** Iteration %g/%g **************\n', istep, 5);
    
    psi = detection_mask - nodetw*(1-detection_mask);

    % initial search direction, normed so that max(abs(search(:))) = 1.0
    [Js,search]=objective_with_gradient(tign,h,'noplot'); 
    search = -search/big(search); 

    plotstate(4,search,'Search direction',0);
    print('-dpng', sprintf('%s_search_dir_%d.png', prefix, istep));
    [Jsbest,best_stepsize] = linesearch(4.0,Js,tign,h,search,4,2);
%    plotstate(21,tign+h+3*search,'Line search (magic step_size=3)',detection_time(1));
    fprintf('Iteration %d: best step size %g\n', istep, best_stepsize);
    if(best_stepsize == 0)
        disp('Cannot improve in this search direction anymore, exiting now.');
        break;
    end
    h = h + best_stepsize*search;
    plotstate(6,tign+h,sprintf('Analysis descent iteration %i [Js=%g]',istep,Jsbest),detection_time(1));
    print('-dpng',sprintf('%s_descent_iter_%d.png', prefix, istep));
    h_stor(:,:,istep) = h;
end
disp('converting analysis fire arrival time from days with zero at the end of the fire to original scale')
analysis=max_tign_g+(24*60*60)*(tign+h); 
disp('input the analysis as tign in WRF-SFIRE with fire_perimeter_time=detection time')

figure(23);
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


    function [J,delta]=objective_with_gradient(tign,h,noplot)
        % compute objective function and optionally ascent direction
        T=tign+h;
        [f0,f1]=like1(psi,detection_time-T,TC*stretch);
        F = f1;             % forcing
        % objective function and preconditioned gradient
        Ah = poisson_fft2(h,[w.dx,w.dy],1);
        % compute both parts of the objective function and compare
        J1 = 0.5*(h(:)'*Ah(:));
        J2 = -ssum(psi.*f0)/(m*n);
        J = alpha*J1 + J2;
        fprintf('Objective function J=%g (J1=%g, J2=%g)\n',J,J1,J2);
        gradJ = alpha*Ah + F;
        fprintf('Gradient: norm Ah %g norm F %g\n', norm(Ah,2), norm(F,2));
        if ~exist('noplot','var'),
            plotstate(7,f0,'Detection likelihood',0.5,'-w');
            plotstate(8,f1,'Detection likelihood derivative',0);
            plotstate(9,F,'Forcing',0); 
            plotstate(10,gradJ,'gradient of J',0);
        end
        delta = solve_saddle(Constr_ign,h,F,@(u) poisson_fft2(u,[w.dx,w.dy],-power)/alpha);
        % plotstate(11,delta,'Preconditioned gradient',0);
        %fprintf('norm(grad(J))=%g norm(delta)=%g\n',norm(gradJ,'fro'),norm(delta,'fro'))
    end


    %NOTE: this function is called only when step size is determined
    %      we are not allowed to modify the objective function here
    %      as this would invalidate the gradient
    function J=objective_only(tign,h)
        T=tign+h;
        [f0,~]=like1(psi,detection_time-T,TC*stretch);
%        F = f1;             % forcing
        % objective function and preconditioned gradient
        Ah = poisson_fft2(h,[w.dx,w.dy],1);
        J1 = 0.5*(h(:)'*Ah(:));
        J2 = -ssum(psi.*f0)/(m*n);
        J = alpha*J1 + J2;
        fprintf('Objective function J=%g (J1=%g, J2=%g), alpha=%g\n',J,J1,J2,alpha);
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
        Jshigh = objective_only(tign,h+max_step*search);
        for d=1:max_depth
            step_sizes = linspace(step_low,step_high,nmesh+2);
            Jsls = zeros(nmesh+2,1);
            Jsls(1) = Jslow;
            Jsls(nmesh+2) = Jshigh;
            for i=2:nmesh+1
                Jsls(i) = objective_only(tign,h+step_sizes(i)*search);
            end
            
            figure(22);
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