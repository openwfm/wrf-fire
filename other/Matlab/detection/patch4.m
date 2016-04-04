% to create conus.kml:
% download http://firemapper.sc.egov.usda.gov/data_viirs/kml/conus_hist/conus_20120914.kmz
% and gunzip 
% 
% to create w.mat:
% run Adam's simulation, currently results in
%
% /share_home/akochans/WRF341F/wrf-fire/WRFV3/test/em_utfire_1d_med_4km_200m
% then in Matlab
% arrays needed only once
% f='wrfout_d01_2013-08-20_00:00:00'; 
% t=nc2struct(f,{'Times'},{});  n=size(t.times,2)  
% w=nc2struct(f,{'Times','TIGN_G','FXLONG','FXLAT','UNIT_FXLAT','UNIT_FXLONG','XLONG','XLAT','NFUEL_CAT'},{},n);
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

% run patch_load first

% figures
figmap=1;
printing=0;
historical='all';
%historical='previous';

% convert tign_g to datenum 
w.time=datenum(char(w.times)');
red.tign=(red.tign_g - max(red.tign_g(:)))/(24*60*60) + w.time;
min_tign=min(red.tign(:));
max_tign=max(red.tign(:));

cmap2=cmap;
cmap2(1:7,:)=NaN;
[cmap,imax]=cmapmod14;
granules{2}=[];
figure(figmap);clf
clear M
for step=2:length(ss.time)  % over WRF frames
    figure(figmap);clf;hold off
    granules{1}=find(r.time <= ss.time(step));
    switch historical
        case 'all' 
            % take all satellite overpasses from the beginning of time
            granules{2}=granules{1};
        case 'last'
            % last time step only
            granules{2}=find(r.time <= ss.time(step) & r.time > ss.time(step-1));
        case 'previous'
            % the previous granules persist until replaced
            new_det=find(r.time <= ss.time(step) & r.time > ss.time(step-1));
            if new_det,
                granules{2}=new_det;
            end
        otherwise
            historical
            error('unknown parameter historical')
    end
    for ipass=1:2,
        det=granules{ipass};
        ndet=length(det);
        if printing>1,
            if det, fprintf('pass %i using satellite granules ',ipass),disp(granules{1}), end
            for idet=1:ndet
                fprintf('%i %s\n',det(idet),datestr(r.time(det(idet))))
            end
        end
        % detections to now
        for idet=1:ndet,
            x=r.x{det(idet)}; % load fire detection image 
            age=t-r.time(idet); % age of detection in days
            offset = min(imax,floor(4*age)); % offset in colormap for detection age
            dd=x.data(:)>6;  % indices of detected
            x.data(dd)=x.data(dd)+3*offset;   % transition to yellow
            if printing>1,
                fprintf('step %i pass %i granule %i detections %i\n',step,ipass,idet,sum(dd))
            end
            if ipass==1, % build up the background
               showmod14(x)
            elseif any(dd), % all except fire transparent
               x.data(~dd)=0;
               showmod14(x)
            end
            hold on
        end % idet
    end % ipass
    u=ss.uh(:,:,step);
    v=ss.vh(:,:,step);
    maxw=max(sqrt(u(:).*u(:)+v(:).*v(:)));
    fprintf('step %i max windspeed %g granules %i\n',step,maxw,ndet)
    sc=0.006;quiver(w.xlong,w.xlat,sc*u,sc*v,0); % wind
    hold on
    t=ss.time(step);
    contour(red.fxlong,red.fxlat,red.tign,[t t],'-k'); % fireline
    title(datestr(t));   
    axis(red.axis)
    avg_lat=0.5*(red.min_lat+red.max_lat);
    daspect([1,cos(avg_lat*pi/180),1]);
    hold off
    drawnow
    pause(0.1)
    M(step-1)=getframe(gcf);
    title(datestr(t));
end % step

mov2mpeg(M,'M')
