v=read_fire_kml('conus.kml');
min_lat=48+4/60+1/360
max_lat= 48+10/60+51/3600
min_lon= - (119+24/60+50/3600)
max_lon= - (119+0/60+35/3600)
bi=find(v.lon > min_lon & v.lon < max_lon & v.lat > min_lat & v.lat < max_lat);
lon=v.lon(bi);
lat=v.lat(bi);
tim=v.tim(bi)-min(v.tim(bi));
figure(1)
plot3(lon,lat,tim,'o'),grid
m=100;
n=100;
mesh_lon=min_lon+[0:m]*(max_lon-min_lon)/m;
mesh_lat=min_lat+[0:n]*(max_lat-min_lat)/n;
[mesh_lon,mesh_lat]=ndgrid(mesh_lon,mesh_lat);
f=TriScatteredInterp(lon,lat,tim,'natural');
mesh_tim=f(mesh_lon,mesh_lat);
hold on, mesh(mesh_lon,mesh_lat,mesh_tim), hold off
title('Barker Canyon fire VIIRS fire detection')
zlabel('days')
ylabel('latitude')
xlabel('longitude')