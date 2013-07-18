function [long,lat,ros,A]=read_file_perimeter(data,wrfout,m,n,time)

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

p=nc2struct(wrfout,{'UNIT_FXLONG','UNIX_FXLAT','FXLONG','FXLAT','TIGN_G'},{},time)


fxlong=fxlong*unit_fxlong;
fxlat=fxlat*unit_fxlat;

ros=read_data_from_wrfout(wrfout,m,n,time);

A=read_perim_from_tign(tign_g);

end

function A=read_perim_from_tign(tign);
A=[];
format long
max_tign=max(tign(:))
tign_copy=max_tign*ones(size(tign,1)+2,size(tign,2)+2);
tign_copy(2:size(tign,1)+1,2:size(tign,2)+1)=tign;
tign=tign_copy;
for i=2:size(tign,1)-1
    for j=2:size(tign,2)-1
        if (tign(i,j)==max_tign)
            if (max(any((tign(i-1:i+1,j-1:j+1))<max_tign))==1)
            A=[A;[i,j]];
            end
       end
    end
end

function bound=read_perim_from_file(data,unit_long,unit_lat);
fid = fopen(data);
bound = fscanf(fid,'%17g %*1s %17g %*3s',[2 inf]);

bound = bound';
fclose(fid);

bound(:,1)=bound(:,1)*unit_long;
bound(:,2)=bound(:,2)*unit_lat;
end

