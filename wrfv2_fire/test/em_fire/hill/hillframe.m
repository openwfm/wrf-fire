disp('1. install mexnc under the same directory as wrf-fire, then')
disp('2. start matlab in wrf/WRFV3/test/em_fire to set up the paths properly')
disp('3. read a wrfrst file into the workspace as in')
disp('   >> ncload wrfrst_d01_0001-01-01_00:01:00')

% note the variables are read here without the WRF permutation of dimensions 
ideal=1

if ideal,
    u=u_2;
    v=v_2;
    w=w_2;
    ph = ph_2;
    r=[10,10]; % refinement factor
    % do not have xlong and xlat in ideal case, but we made the coordinates up
    % for fxlong and fxlat, so interpolate
    n=size(xlong);
    [xa,ya]=ndgrid(r(1)*[1/2:1:n(1)],r(2)*[1/2:1:n(2)]); % centers of atm cells in fire grid
    axlong = interp2(fxlong,xa,ya); % fake coordinates for display
    axlat = interp2(fxlat,xa,ya); % fake coordinates for display
else
    axlong=xlong; axlat=xlat; % in real case, xlong xlat populated
end

clf
amin=[15,15,1];  % the atm grid part to show
amax=[30,30,1];
amin=[1,1,1];  % the atm grid part to show
amax=[41,41,1];

qstep=[5,5];        % quiver step for wind on the surface fire grid
astep=[1,1,1];      % quiver step for wind on the atmosphere grid
qs=1.5;           % scaling for quiver arrows


%-------------------------------------------------

% compute the corresponding part of the fire grid
dmin=1+r.*(amin(1:2)-1);
dmax=r.*amax(1:2);


% surface wind arrows
ii=dmin(1):qstep(1):dmax(1);jj=dmin(2):qstep(2):dmax(2);
hq=quiver3(fxlong(ii,jj),fxlat(ii,jj),zsf(ii,jj),qs*uf(ii,jj),qs*vf(ii,jj),zeros(size(vf(ii,jj))),0);
hold on
i=dmin(1):dmax(1);j=dmin(2):dmax(2);

% surface colored by heat flux
hs=surf(fxlong(i,j),fxlat(i,j),zsf(i,j),fgrnhfx(i,j),'EdgeColor','none'); colorbar
[c,hc]=contour3(fxlong(i,j),fxlat(i,j),zsf(i,j));
for h=hc(:)', set(h,'EdgeColor','black');end

err_hgt = big(hgt-phb(:,:,1)/9.81)  % the lowest background geopotential height is terrain height

% geopotential altitude of the centers of lower cell face
% ph is perturbation from the background phb
% note ph and phb are staggered just as w
a = (ph+phb)/9.81;

% interpolate the geopotential and w velocity from staggered grid to cell centers
ac = (a(:,:,1:end-1)+a(:,:,2:end))/2;
wc = (w(:,:,1:end-1)+w(:,:,2:end))/2; 
% interpolate u and v components of wind velocity from staggered grid to cell centers
uc = (u(1:end-1,:,:)+u(2:end,:,:))/2;  % from front and rear face to the center
vc = (v(:,1:end-1,:)+v(:,2:end,:))/2;  % from front and rear face to the center

% atmosphere wind arrows
ia=amin(1):astep(1):amax(1);
ja=amin(2):astep(2):amax(2);
for k=amin(3):astep(3):amax(3),
    hqa=quiver3(axlong(ia,ja),axlat(ia,ja),ac(ia,ja,k),... % position
        qs*u(ia,ja,k),qs*v(ia,ja,k),qs*w(ia,ja,k),0); % wind arrow
end

caxis([0,1e6]);   % for heat flux color
axis equal
xlabel('x (m)')
ylabel('y (m)')
zlabel('z (m)')
hold off
drawnow