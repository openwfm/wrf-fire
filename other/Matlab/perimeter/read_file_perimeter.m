function [long,lat,uf,vf,dzdxf,dzdyf,time_now,bound]=read_file_perimeter(data,data_long,data_lat,data_uf,data_vf,data_dzdxf,data_dzdyf,time)

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

long=dlmread('data_LONG.txt');
lat=dlmread('data_LAT.txt');
uf=dlmread('data_UF.txt');
vf=dlmread('data_VF.txt');
dzdxf=dlmread('data_DZDXF.txt');
dzdyf=dlmread('data_DZDYF.txt');


fid = fopen(data);
data = fscanf(fid,'%21g %*1s %19g %*3s \n',[2 inf]);
data = data';
fclose(fid)
data_size=size(data);

time_now=data(1,1);  
unit_long=9.3206484e+04;
unit_lat=1.1117746e+05;

bound=data(2:data_size(1),:); 
bound(:,1)=bound(:,1)*unit_long;
bound(:,2)=bound(:,2)*unit_lat;

%plot(bound(:,1),bound(:,2),'-')



