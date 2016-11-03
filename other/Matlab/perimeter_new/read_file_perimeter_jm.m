function [fxlong,fxlat,fire_perimeter,timestep_end]=read_file_perimeter(wrfout,wrfout_fire,time,input_type,input_file)
% Volodymyr Kondratenko           April 3 2012

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Input: wrfout : String with the name of the wrfout file,  
%                  It is needed for reading the latitude and longtitude
%                  coordinates of the mesh and also UNIT_FXLONG and
%                  UNIT_FXLAT variables
%        time    : time step index in the wrfout (last index)
%                  example FXLAT(:,:,time)  
% Output: 
%        p 	structure with various fiels
%        long = FXLONG, longtitude coordinates of the mesh converted to (m)
%        lat = FXLAT, latitude coordinates of the mesh converted to (m)
%        fire_area = fire map,[0,1] array, where 0- not
%                  burning area, >0 burning area, 1 - area that was burnt
%                  If input_type=1, then fire_area - set of ordered points 
%                  of the boundary 1st=last;
%                  bound(i,1)-horisontal; bound(i,1)-vertical coordinate

datafile=sprintf('data_%s_%i_%i',wrfout,time,input_type);
global saved_data  % 0 = read from original files and store in matlab files, 1=read saved data 
disp(['read_file_perimeter time=',num2str(time),' input_type=',num2str(input_type)]);

if (input_type==0)
    error('not supported')
    if saved_data
        w=load(datafile);
        p=w.p; q=w.q;
    else
        p=nc2struct(wrfout,{'UNIT_FXLONG','UNIT_FXLAT','FXLONG','FXLAT','Times','ITIMESTEP'},{'DT'},time);
        q=nc2struct(wrfout_fire,{'FIRE_AREA'},{},time);    
        save(datafile,'p','q')
    end
    fire_area=q.fire_area;
    fxlong=p.fxlong*p.unit_fxlong;
    fxlat=p.fxlat*p.unit_fxlat;
elseif (input_type==1) 
    error('not supported')
    if saved_data
        w=load(datafile);
        p=w.p; 
    else
        p=nc2struct(wrfout,{'UNIT_FXLONG','UNIT_FXLAT','FXLONG','FXLAT'},{},time);
        save(datafile,'p')
    end
    fxlong=p.fxlong*p.unit_fxlong;
    fxlat=p.fxlat*p.unit_fxlat;
    fire_area=read_perim_from_file(input_file,fxlong,fxlat,p.unit_fxlong,p.unit_fxlat);

% Test
    fid = fopen('output_fire_area.txt', 'w');
    dlmwrite('output_fire_area.txt', fire_area, 'delimiter', '\t','precision', '%.4f');
    fclose(fid);
elseif (input_type==2)
    if saved_data
	disp(['loading from ',datafile])
        w=load(datafile);
        p=w.p; 
    else
        p=nc2struct(wrfout,{'UNIT_FXLONG','UNIT_FXLAT','FXLONG','FXLAT','ITIMESTEP'},{'DT','DX','DY'},time);
	disp(['storing to ',datafile])
        save(datafile,'p')
    end
    disp(['reading fire_perimeter_big from ',input_file,' frame ',num2str(time)])
    fire_perimeter_big=dlmread(input_file);
    fxlong=p.fxlong*p.unit_fxlong;
    fxlat=p.fxlat*p.unit_fxlat;
    fire_area=zeros(size(fire_perimeter_big(:,1:2)));
    fire_perimeter(:,1)=fire_perimeter_big(:,1)*p.unit_fxlong;
    fire_perimeter(:,2)=fire_perimeter_big(:,2)*p.unit_fxlat;
    timestep_end=p.itimestep*p.dt;
    %fire_area=fire_area_big(:,1:2);
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

bound(:,1)=bound(:,1)*unit_long;
bound(:,2)=bound(:,2)*unit_lat;

xv=bound(:,1);
yv=bound(:,2);
xv=xv*100000;
yv=yv*100000;
lat1=lat*100000;
long1=long*100000;
[IN,ON] = inpolygon(long1,lat1,xv,yv);

max(max(IN(:,:)))

end

