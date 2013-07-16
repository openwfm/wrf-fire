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
wrfout='wrfout_d05_2012-09-09_00:00:00';
time=281;
long=dlmread('data_LONG.txt');
lat=dlmread('data_LAT.txt');
%uf=dlmread('data_UF.txt');
%vf=dlmread('data_VF.txt');
dzdxf=dlmread('data_DZDXF.txt');
dzdyf=dlmread('data_DZDYF.txt');




time_now=216000;

fid = fopen(data);
bound = fscanf(fid,'%17g %*1s %17g %*3s',[2 inf]);
% data = fscanf(fid,'%21g %*1s %19g %*3s \n',[2 inf]);

bound = bound';
fclose(fid)

unit_long=7.4518492e+04;
unit_lat=1.1117746e+05;
bound(:,1)=bound(:,1)*unit_long;
bound(:,2)=bound(:,2)*unit_lat;


ncid = netcdf.open(wrfout,'NC_NOWRITE');
varid = netcdf.inqVarID(ncid,char('UF'));
uf=netcdf.getVar(ncid,varid,[0,0,time],[3920,3860,1]);

varid = netcdf.inqVarID(ncid,char('VF'));
vf=netcdf.getVar(ncid,varid,[0,0,time],[3920,3860,1]);


varid = netcdf.inqVarID(ncid,char('F_ROS'));
max_ros_wrf=netcdf.getVar(ncid,varid,[0,0,time],[3920,3860,1]);

fid = fopen('max_ros_from_wrf.txt', 'w');
    dlmwrite('max_ros_from_wrf.txt', max_ros_wrf, 'delimiter', '\t','precision', '%.4f');
    fclose(fid);

size(bound)
bound(100,:)




