function B=read_file_ignition(data,wrfout)

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
% Output: 
%        unit_long = 
%        unit_lat =
%        long =
%        lat = 
%        time_now =
%        mesh_size =
%        ign_pnt
%        bound - set of ordered points of the boundary 1st=last 
%        bound(i,1)-horisontal; bound(i,1)-vertical coordinate




format long
unit_long=ncread(wrf_out,'UNIT_FXLONG');
unit_lat=ncread(wrf_out,'UNIT_FXLAT');
unit_long=unit_long(1);
unit_lat=unit_lat(1);
long=ncread(wrf_out,'FXLONG');
lat=ncread(wrf_out,'FXLAT');
long=long*unit_long;
lat=lat*unit_lat;

fid = fopen(data);
data = fscanf(fid,'%g %g',[2 inf]); % It has two rows now.
data = data';
fclose(fid)
data_size=size(data);

time_now=data(1,1);  % time of ignition on the boundary 
mesh_size=data(2,:); % size of the matrix
ign_pnt=data(3,:);   % coordinates of the ignition point    
ign_pnt(1)=ign_pnt(1)*unit_long;
ign_pnt(2)=ign_pnt(2)*unit_lat;
bound=data(4:data_size(1),:); 
bound(:,1)=bound(:,1)*unit_long;
bound(:,2)=bound(:,2)*unit_lat;

% bound - set of ordered points of the boundary 1st=last 
% bound(i,1)-horisontal; bound(i,1)-vertical coordinate


