function M=patch2
% to create w.mat:
% run Adam's simulation, currently results in
%
% /share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_200m
% then in Matlab
% arrays needed only once
% f='wrfout_d01_2013-08-20_00:00:00'; 
% t=nc2struct(f,{'Times'},{});  n=size(t.times,2)  
% w=nc2struct(f,{'Times','TIGN_G','FXLONG','FXLAT','UNIT_FXLAT','UNIT_FXLONG','XLONG','XLAT','NFUEL_CAT'},{'DX','DY'},n);
% save ~/w.mat w    
%
% array at fire resolution every saved timestep
% to create s.mat:
% a=dir('wrfout_d01*');
% s=read_wrfout_sel({a.name},{'FGRNHFX',Times}); 
% save ~/s.mat s 
% 
% arrays at atm resolution every saved timestep
% to create ss.mat
% a=dir('wrfout_d01*')
% s=read_wrfout_sel({a.name},{'Times','UAH','VAH'})
% save ss s
% 
% fuels.m is created by WRF-SFIRE at the beginning of the run

% ****** REQUIRES Matlab 2013a - will not run in earlier versions *******

% figures
figmap=2;
fig3d=0;

load w

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
% load s
% load c
fuels

% establish boundaries from simulations

sim.min_lat = min(w.fxlat(:))
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

% restrict data for display

red.fxlat=w.fxlat(ispan,jspan);
red.fxlong=w.fxlong(ispan,jspan);
red.tign_g=w.tign_g(ispan,jspan);
red.nfuel_cat=w.nfuel_cat(ispan,jspan);

red.min_lat = min(red.fxlat(:))
red.max_lat = max(red.fxlat(:))
red.min_lon = min(red.fxlong(:))
red.max_lon = max(red.fxlong(:))

% convert tign_g to datenum 
w.time=datenum(char(w.times)');
red.tign=(red.tign_g - max(red.tign_g(:)))/(24*60*60) + w.time;
min_tign=min(red.tign(:));
max_tign=max(red.tign(:));


red.tign(find(red.tign==max_tign))=NaN; % squash the top
if fig3d>0,
    figure(fig3d); clf
    hold off
    h=surf(red.fxlong,red.fxlat,red.tign-min_tign); 
    xlabel('longitude'),ylabel('latitude'),zlabel('days')
    set(h,'EdgeAlpha',0,'FaceAlpha',0.5); % show faces only
    drawnow
end

prefix='TIFs/';    % the level2 files processed by geotiff2mat.py
p=sort_rsac_files(prefix);
d=p.file;

cmap=cmapmod14;
cmap2=cmap;
cmap2(1:7,:)=NaN;
plot_all_level2=true;

figure(figmap);clf
iframe=1;
for i=1:length(d),
    file=d{i};
    v=readmod14(prefix,file);
    % select fire detection within the domain
    xj=find(v.lon > red.min_lon & v.lon < red.max_lon);
    xi=find(v.lat > red.min_lat & v.lat < red.max_lat);
    ax=[red.min_lon red.max_lon red.min_lat red.max_lat];
    if ~isempty(xi) & ~isempty(xj)
        x=[];
        x.axis=[red.min_lon,red.max_lon,red.min_lat,red.max_lat];
        x.file=v.file; 
        x.time=v.time;
        x.data=v.data(xi,xj);    % subset data
        x.lon=v.lon(xj);
        x.lat=v.lat(xi);
        [x.xlon,x.xlat]=meshgrid(x.lon,x.lat);
        if any(x.data(:)>6), % some data present - not all absent, water, or cloud
            figure(figmap);clf
            showmod14(x)
            hold on
            contour(red.fxlong,red.fxlat,red.tign,[v.time v.time],'-k');
            fprintf('image time            %s\n',datestr(x.time));
            fprintf('min wind field time   %s\n',datestr(ss.min_time));
            fprintf('max wind field time   %s\n',datestr(ss.max_time));
            if x.time>=ss.min_time && x.time <= ss.max_time,
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
            hold off
            axis(ax)
            drawnow
            M(iframe)=getframe(gcf);
            iframe=iframe+1;
            print(figmap,'-dpng',['fig',v.timestr],'-r1600'); 
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
    end
end

end