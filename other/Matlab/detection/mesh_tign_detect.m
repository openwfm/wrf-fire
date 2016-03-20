function mesh_tign_detect(fig,fxlong,fxlat,tign,v,str)
% 3d ignition time and detections

% reduced resolution 3d plot
res=500;
figure(fig)
clf, hold off
[m,n]=size(fxlong);
m_plot=min(m,res); n_plot=min(n,res);
m1=1;m2=m;n1=1;n2=n;
mi=m1:ceil((m2-m1+1)/m_plot):m2; % reduced index vectors
ni=n1:ceil((n2-n1+1)/n_plot):n2;
mesh_fxlong=fxlong(mi,ni);
mesh_fxlat=fxlat(mi,ni);
mesh_tign=tign(mi,ni);
[mesh_m,mesh_n]=size(mesh_fxlat)
% mesh_tign(mesh_tign(:)==max(mesh_tign(:)))=NaN;
surf(mesh_fxlong,mesh_fxlat,mesh_tign,'EdgeAlpha',0,'FaceAlpha',0.5)
grid on
zlabel('Days')
ylabel('Latitude')
xlabel('Longitude')

% units
eradius=6370;
unit_fxlat = eradius*2*pi/360;
lon_ctr=mean(fxlong(:));
unit_fxlong = unit_fxlat*cos(lon_ctr*2*pi/360);
% plot black patches as detection squares as patches
if exist('v','var'),
    c=cmapmod14;
    min_fxlon=min(fxlong(:));
    max_fxlon=max(fxlong(:));
    min_fxlat=min(fxlat(:));
    max_fxlat=max(fxlat(:));
    isel=find(v.lon > min_fxlon & v.lon< max_fxlon ...
            & v.lat > min_fxlat & v.lat< max_fxlat);
    ndet=length(isel);
    X=zeros(4,ndet);Y=X;Z=X;
    for ix=1:ndet,
        i=isel(ix);
        res=v.res(i)*1e-3;
        dx2=0.5*res/unit_fxlong;
        dy2=0.5*res/unit_fxlat;
        X(:,ix)=[v.lon(i)-dx2,v.lon(i)+dx2,v.lon(i)+dx2,v.lon(i)-dx2]';
        Y(:,ix)=[v.lat(i)-dy2,v.lat(i)-dy2,v.lat(i)+dy2,v.lat(i)+dy2]';
        Z(:,ix)=v.tim(i);
    end
    hold on
    patch(X,Y,Z,'black');
    hold off
end
title(str)
grid on,drawnow
fprintf('Figure %i %s\n',fig,str)
end 
