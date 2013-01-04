function [long,lat,uf,vf,dzdxf,dzdyf,time_now,bound]=read_file_perimeter(data,wrfout,time)

% Volodymyr Kondratenko           April 3 2012

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Input:    data : String - data, that contains the name of the Text file.
%                  First 2 columns - coordinates of all the
%                  points on the boundary (lon,lat). 
%                  1rt row - time_now (second number is not needed, is set to 0);
%                  2nd row - size of the mesh;
%                  3rd row - coordinates of ignition point;
%                  All next rows - coordinates of all the
%                  points on the boundary (lon,lat). %       
%         wrfout : String with the name of the wrfout file,  
%                  It is needed for reading the latitude and longtitude
%                  coordinates of the mesh and also UNIT_FXLONG and
%                  UNIT_FXLAT variables
%        time    : coordinate in the wrfout array, that needs to be computed
%                  example FXLAT(:,:,time)  
% Output: 
%        unit_long = UNIT_FXLONG;
%        unit_lat = UNIT_FXLAT;
%        long = FXLONG, longtitude coordinates of the mesh
%        lat = FXLAT, latitude coordinates of the mesh 
%        time_now = time of ignition on the boundary
%        mesh_size = size of the mesh
%        ign_pnt - point of ignition
%        bound - set of ordered points of the boundary 1st=last 
%        bound(i,1)-horisontal; bound(i,1)-vertical coordinate




format long
unit_long=ncread(wrfout,'UNIT_FXLONG');
unit_lat=ncread(wrfout,'UNIT_FXLAT');
uf=ncread(wrfout,'UF');
uf=uf(:,:,time);
vf=ncread(wrfout,'VF');
vf=vf(:,:,time);
'uf(1000,1000)'  
 uf(1000,1000)
'vf(1000,1000)' 
vf(1000,1000)
dzdxf=ncread(wrfout,'DZDXF');
dzdxf=dzdxf(:,:,time);
dzdyf=ncread(wrfout,'DZDYF');
dzdyf=dzdyf(:,:,time);
'dzdxf(1000,1000)' 
dzdxf(1000,1000)
'dzdyf(1000,1000)' 
dzdyf(1000,1000)
unit_long=unit_long(1);
unit_lat=unit_lat(1);
'unit_long' 
unit_long
'unit_lat' 
unit_lat
long=ncread(wrfout,'FXLONG');
long=long(:,:,time);
lat=ncread(wrfout,'FXLAT');
lat=lat(:,:,time);
long=long*unit_long;
lat=lat*unit_lat;
'long(1000,1000)' 
long(1000,1000)
'lat(1000,1000)' 
lat(1000,1000)

fid = fopen(data);
data = fscanf(fid,'%21g %*1s %19g %*3s \n',[2 inf]);
%data = fscanf(fid,'%g %g',[2 inf]); % It has two rows now.
data = data';
'data(1:3,:)'
data(1:3,:)
fclose(fid)
data_size=size(data);

time_now=data(1,1);  

bound=data(2:data_size(1),:); 
bound(:,1)=bound(:,1)*unit_long;
bound(:,2)=bound(:,2)*unit_lat;
% bound - set of ordered points of the boundary 1st=last 
% bound(i,1)-horisontal; bound(i,2)-vertical coordinate

plot(bound(:,1),bound(:,2),'-')



