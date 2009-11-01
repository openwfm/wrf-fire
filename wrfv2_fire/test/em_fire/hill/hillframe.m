disp('1. install mexnc under the same directory as wrf-fire, then')
disp('2. start matlab in wrf/WRFV3/test/em_fire to set up the paths properly')
disp('3. read a wrfrst file into the workspace as in')
disp('   >> ncload wrfrst_d01_0001-01-01_00:01:00')

clf
dmin=[1,1];
dmax=[410,410];
qstep=[5,5];
dmin=[150,150];
dmax=[300,300];
qstep=[5,5];
% surface wind arrows
ii=dmin(1):qstep(1):dmax(1);jj=dmin(2):qstep(2):dmax(2);
hq=quiver3(fxlong(ii,jj),fxlat(ii,jj),zsf(ii,jj),uf(ii,jj),vf(ii,jj),zeros(size(vf(ii,jj))));
hold on
i=dmin(1):dmax(1);j=dmin(2):dmax(2);
% surface colored by heat flux
hs=surf(fxlong(i,j),fxlat(i,j),zsf(i,j),fgrnhfx(i,j),'EdgeColor','none'); colorbar
[c,hc]=contour3(fxlong(i,j),fxlat(i,j),zsf(i,j));
for h=hc(:)', set(h,'EdgeColor','black');end
axis equal
xlabel('x (m)')
ylabel('y (m)')
zlabel('z (m)')
hold off
drawnow