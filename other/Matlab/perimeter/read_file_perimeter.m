function [fxlong,fxlat,fire_area]=read_file_perimeter(wrfout,time,input_type,input_file)
% Volodymyr Kondratenko           April 3 2012

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Input: wrfout : String with the name of the wrfout file,  
%                  It is needed for reading the latitude and longtitude
%                  coordinates of the mesh and also UNIT_FXLONG and
%                  UNIT_FXLAT variables
%        time    : coordinate in the wrfout array, that needs to be computed
%                  example FXLAT(:,:,time)  
% Output: 
%        long = FXLONG, longtitude coordinates of the mesh converted to (m)
%        lat = FXLAT, latitude coordinates of the mesh converted to (m)
%        fire_area = fire map,[0,1] array, where 0- not
%                  burning area, >0 burning area, 1 - area that was burnt
%                  If input_type=1, then fire_area - set of ordered points 
%                  of the boundary 1st=last;
%                  bound(i,1)-horisontal; bound(i,1)-vertical coordinate



if (input_type==0)
    p=nc2struct(wrfout,{'UNIT_FXLONG','UNIT_FXLAT','FXLONG','FXLAT','FIRE_AREA'},{},time);
    fire_area=p.fire_area;
    fxlong=p.fxlong*p.unit_fxlong;
    fxlat=p.fxlat*p.unit_fxlat;
elseif (input_type==1)
    p=nc2struct(wrfout,{'UNIT_FXLONG','UNIT_FXLAT','FXLONG','FXLAT'},{},time);
    fxlong=p.fxlong*p.unit_fxlong;
    fxlat=p.fxlat*p.unit_fxlat;
    fire_area=read_perim_from_file(input_file,fxlong,fxlat,p.unit_fxlong,p.unit_fxlat);

% Test
    fid = fopen('output_fire_area.txt', 'w');
    dlmwrite('output_fire_area.txt', fire_area, 'delimiter', '\t','precision', '%.4f');
    fclose(fid);
elseif (input_type==2)
    p=nc2struct(wrfout,{'UNIT_FXLONG','UNIT_FXLAT','FXLONG','FXLAT'},{},time);
    fxlong=p.fxlong*p.unit_fxlong;
    fxlat=p.fxlat*p.unit_fxlat;
    fire_area=dlmread(input_file);
end

end


% Input                 data : String - data, that contains the name of the Text file.
%                  First 2 columns - coordinates of all the
%                  points on the boundary (lon,lat). 
%                  1rt row - time_now (second number is not needed, is set to 0);
%                  2nd row - size of the mesh;
%                  3rd row - coordinates of ignition point;
%                  All next rows - coordinates of all the
%                  points on the boundary (lon,lat). %   
% Output           bound - set of ordered points of the boundary 1st=last 
%                  bound(i,1)-horisontal; bound(i,1)-vertical coordinate


function IN=read_perim_from_file(data,long,lat,unit_long,unit_lat)
fid = fopen(data);
bound = fscanf(fid,'%9g %*1s %8g %*3s',[2 inf]);
% bound - set of ordered points of the boundary 1st=last 
%         bound(i,1)-horisontal; bound(i,1)-vertical coordinate

bound = bound';
fclose(fid);

'bound before *unit'
bound(1:10,:)

bound(:,1)=bound(:,1)*unit_long;
bound(:,2)=bound(:,2)*unit_lat;

'bound after *unit'
bound(1:10,:)

'unit_long'
unit_long
'unit_lat'
unit_lat

xv=bound(:,1);
yv=bound(:,2);
xv=xv*100000;
yv=yv*100000;
lat1=lat*100000;
long1=long*100000;
'lat1(1000,1000)'
lat1(1000,1000)
'long1(1000,1000)'
long1(1000,1000)
[IN,ON] = inpolygon(long1,lat1,xv,yv);
'IN (1000,1000)'
IN(1000,1000)
IN(100,100)
IN(10,10)

max(max(IN(:,:)))

end

