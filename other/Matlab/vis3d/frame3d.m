function frame3d(swind,amin,amax,astep,qstep,qs,...
    fxlong,fxlat,xlong,xlat,zsf,fgrnhfx,uf,vf,u,v,w,ph,phb,hgt)

clf,hold off

r = size(fxlong)./(size(xlong));  % refinement ratio
% see if xlong and xlat are bogus
ideal = all(xlong(:) ==0)|any(abs(xlong(:))>180)|any(abs(xlat(:))>180);

if ideal,
    % do not have xlong and xlat in ideal case, but we made the coordinates up
    % for fxlong and fxlat, so interpolate
    n=size(xlong);
    [xa,ya]=ndgrid(r(1)*[1/2:1:n(1)],r(2)*[1/2:1:n(2)]); % centers of atm cells in fire grid
    axlong = interp2(fxlong,xa,ya); % fake coordinates for display
    axlat = interp2(fxlat,xa,ya); % fake coordinates for display
    xscale=1;
    yscale=1;
else
    axlong=xlong; axlat=xlat; % in real case, xlong xlat populated
        er=6378e3; % earth radius in m
    deg2rad=2*pi/360; % 1 degree in radians
    % degrees to meters, long and lat
    lat_deg2m=er*deg2rad;
    long_deg2m=er*deg2rad*cos(deg2rad*mean(xlat(find(xlat))));
    xscale=1/long_deg2m;
    yscale=1/lat_deg2m;
end
aspect_ratio = [xscale yscale 1];

% compute the corresponding part of the fire grid
dmin=1+r.*(amin(1:2)-1);
dmax=r.*amax(1:2);

% surface colored by heat flux
i=dmin(1):dmax(1);j=dmin(2):dmax(2);
hs=surf(fxlong(i,j),fxlat(i,j),zsf(i,j),fgrnhfx(i,j),'EdgeColor','none','FaceAlpha',0.7); 
caxis([0,1e6]);   % for heat flux color
axis tight, colorbar
hold on
[c,hc]=contour3(fxlong(i,j),fxlat(i,j),zsf(i,j));
for h=hc(:)', set(h,'EdgeColor','black');end  % color the contours black


% surface wind arrows
if swind,
    ii=dmin(1):qstep(1):dmax(1);jj=dmin(2):qstep(2):dmax(2);
    hq=quiver3(fxlong(ii,jj),fxlat(ii,jj),zsf(ii,jj),...
        xscale*uf(ii,jj),yscale*vf(ii,jj),zeros(size(vf(ii,jj))),qs);
    set(hq,'Color','black')
end

% geopotential altitude of the centers of lower cell face
% ph is perturbation from the background phb
% note ph and phb are staggered just as w
a = (ph+phb)/9.81;
% check if the lowest background geopotential height is terrain height
err_hgt = big(hgt-phb(:,:,1)/9.81)  
% interpolate the geopotential and w velocity from staggered grid to cell centers
ac = (a(:,:,1:end-1)+a(:,:,2:end))/2;
wc = (w(:,:,1:end-1)+w(:,:,2:end))/2; 
% interpolate u and v components of wind velocity from staggered grid to cell centers
uc = (u(1:end-1,:,:)+u(2:end,:,:))/2;  % from front and rear face to the center
vc = (v(:,1:end-1,:)+v(:,2:end,:))/2;  % from front and rear face to the center

% atmosphere wind arrows
ia=amin(1):astep(1):amax(1);
ja=amin(2):astep(2):amax(2);
colors={'red','green','blue','black'};
for k=amin(3):astep(3):amax(3),
    q1=u(ia,ja,k)*xscale;
    q2=v(ia,ja,k)*yscale;
    q3=w(ia,ja,k);
    hqa=quiver3(axlong(ia,ja),axlat(ia,ja),ac(ia,ja,k),q1,q2,q3,qs);
    set(gca,'DataAspectRatio',[xscale yscale 1]);
    i=1+mod(k-1,length(colors));
    set(hqa,'Color',colors{i});
end

if ideal
    xlabel('x (m)')
    ylabel('y (m)')
else
    % set(gca,'PlotBoxAspectRatioMode','auto');
    xlabel('longitude (deg)')
    ylabel('latitutude (deg)')
end
zlabel('z (m)')
set(gca,'DataAspectRatio',[xscale yscale 1]);
hold off
drawnow

end