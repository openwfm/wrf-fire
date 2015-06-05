function analysis=detect_fit
% from a copy of barker2

disp('input data')
    % to create conus.kml:
    % download http://firemapper.sc.egov.usda.gov/data_viirs/kml/conus_hist/conus_20120914.kmz
    % and gunzip 
    % 
    % to create w.mat:
    % run Adam's simulation, currently results in
    % /home/akochans/NASA_WSU/wrf-fire/WRFV3/test/em_barker_moist/wrfoutputfiles_live_0.25
    % then in Matlab
    % f='wrfout_d05_2012-09-15_00:00:00'; 
    % t=nc2struct(f,{'Times'},{'DX','DY'});  n=size(t.times,2);  w=nc2struct(f,{'TIGN_G','FXLONG','FXLAT','UNIT_FXLAT','UNIT_FXLONG'},{},n);
    % save ~/w.mat w    
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
    
    
    conus = input_num('0 for viirs, 1 for modis',0);
    if conus==0, 
            v=read_fire_kml('conus_viirs.kml');
            detection='VIIRS';
    elseif conus==1,
            v=read_fire_kml('conus_modis.kml');
            detection='MODIS';
    else
            error('need kml file')
    end
        
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
    for i=1:length(default_bounds),fprintf('default bounds %i: %8.5f %8.5f %8.5f %8.5f\n',i,default_bounds{i});end
    
    bounds=input_num('bounds [min_lon,max_lon,min_lat,max_lat] or number of bounds above',1);
    if length(bounds)==1, bounds=default_bounds{bounds}; end
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
    detection_bounds=input_num('detection bounds as [upper,lower]',...
        [u_in(i)-min_tign-tol,u_in(i)-min_tign+tol]);
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
    plotstate(1,detection_mask,['Fire detection at ',detection_datestr],[])
    % add ignition point
    hold on, plot(w.fxlong(i_ign,j_ign),w.fxlat(i_ign,j_ign),'xw'); hold off
    % legend('first ignition at %g %g',w.fxlong(i_ign,j_ign),w.fxlat(i_ign,j_ign))
    
    fuelweight(length(fuel)+1:max(c.nfuel_cat(:)))=NaN;
    for j=1:length(fuel), 
        fuelweight(j)=fuel(j).weight;
    end
    W = zeros(m,n);
    for j=1:n, for i=1:m
           W(i,j)=fuelweight(c.nfuel_cat(i,j));
    end,end
 
    plotstate(2,W,'Fuel weight',[])
        
disp('optimization loop')
h =zeros(m,n); % initial increment
plotstate(3,tign,'Forecast fire arrival time',detection_time(1));
for istep=1:5
    
    % can change the objective function here
    alpha=input_num('penalty coefficient alpha, <0 to end',1e-2);
    if alpha<0, break, end
    % TC = W/(900*24); % time constant = fuel gone in one hour
    TC = 1/24;  % detection time constants in hours
    stretch=input_num('Tmin,Tmax,Tneg,Tpos',[0.5,10,5,10]);
    nodetw=input_num('no fire detection weight',0.1);
    power=input_num('negative laplacian power',0.51);
    
    psi = detection_mask - nodetw*(1-detection_mask);

    [Js,search]=objective(tign,h); 
    search = -search/big(search); % initial search direction

    plotstate(4,search,'Search direction',0);
    h=zeros(m,n); % initial increment
    stepsize=0;
    % initial estimate of stepsize
    last_stepsize = 3;
    for i=2:100 % crude manual line search
        s=input_num('step size',last_stepsize);
        stepsize(i)=s;
        last_stepsize=s;
        plotstate(5,tign+h+last_stepsize*search,'Line search',detection_time(1));
        [Js(i),delta]=objective(tign,h+last_stepsize*search,'noplot');
        c=input_num('try another step size: 0/1',1)
        if c==0, break, end
    end
    h = h + last_stepsize*search;
    plotstate(6,tign+h,sprintf('Analysis descent iteration %i',istep),detection_time(1));
end
disp('converting analysis fire arrival time from days with zero at the end of the fire to original scale')
analysis=max_tign_g+(24*60*60)*(tign+h); 
disp('input the analysis as tign in WRF-SFIRE with fire_perimeter_time=detection time')

    function [J,delta]=objective(tign,h,noplot)
    % compute objective function and optionally ascent direction
    T=tign+h;
    [f0,f1]=like1(psi,detection_time-T,TC*stretch);
    F = f1;             % forcing
    % objective function and preconditioned gradient
    Ah = poisson_fft2(h,[w.dx,w.dy],1);
    J = alpha*0.5*(h(:)'*Ah(:)) - ssum(psi.*f0)/(m*n);
    fprintf('Objective function J=%g\n',J);
    gradJ = alpha*Ah + F;
    if ~exist('noplot','var'),
        plotstate(7,f0,'Detection likelihood',0.5,'-w');
        plotstate(8,f1,'Detection likelihood derivative',0);
        plotstate(9,F,'Forcing',0); 
        plotstate(10,gradJ,'gradient of J',0);
    end
    delta = solve_saddle(Constr_ign,h,F,@(u) poisson_fft2(u,[w.dx,w.dy],-power)/alpha);
    % plotstate(11,delta,'Preconditioned gradient',0);
    fprintf('norm(grad(J))=%g norm(delta)=%g\n',norm(gradJ,'fro'),norm(delta,'fro'))
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

end % detect_fit

function i=map_index(x,a,b,n)
% find image of x under linear map [a,b] -> [1,m]
% and round to integer
i=round(1+(n-1)*(x-a)/(b-a));
end