function tign=perimeter_in_jm(long,lat,fire_area,wrfout,time,interval,time_step,num_wrf, input_type)

% Volodymyr Kondratenko           July 19 2013	
% Rebuilt Jan Mandel May 2016

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Input:    
%    long,lat      longtitude and latitude converted to meters
%    fire_area     fire map,[0,1] array, where 0- not
%                  burning area, >0 burning area, 1 - area that was burnt
%    wrfout        name of the wrfout file, that is being used to read ros
%    time_now      the perimeter time (s)
%    time          update the wind every count time steps
%    interval      time step in wrfout in seconds
%
% Output: 
%    Final Matrix of times of ignition will be printed to 'output_tign.txt' % JM use save tign instead, create tign.mat

% Algorithm state is stored in arrays A D C
%
% A contains rows [i,j] of indices of nodes not burning that have at least one burning neighbor
%   and time of ignition > time_now 
% Computing 4d array of distances between a point and its 8 neighbors
%ideas
% Change to this later (if it speeds up the algorithm)
               % [row,col]=find(C(A(jj,1)-1:A(jj,1)+1,A(jj,2)-1:A(jj,2)+1)==0);
               % for index=1:size(row,1)
               %   dx=row(index)-2;
               %   dy=col(index)-2;
% Iine 180, maybe I hould do it each step toavoid extra calculations
data_steps='started perimeter_in';

distance=get_distances(long,lat);

%case when the fire_area is given in a file
if (input_type==2) 
 xv=fire_area(:,1);
 yv=fire_area(:,2);
 xv=xv*100000;
 yv=yv*100000;
 lat1=lat*100000;
 long1=long*100000;
 [IN,ON] = inpolygon(long1,lat1,xv,yv); 
 fire_area = IN(:,:,1);
end

tign=get_tign_from_dif_eq(wrfout,fire_area,distance,time,interval,time_step,num_wrf,long,lat);

fid = fopen('output_tign_test.txt', 'w');
    dlmwrite('output_tign_test.txt', tign, 'delimiter', '\t','precision', '%.4f');
    fclose(fid);
    
end


function [tign]=get_tign_from_dif_eq(wrfout,fire_area,distance,time,interval,time_step,num_wrf,long,lat)

time_now=interval*(time_step*(num_wrf-1)+time); % because time starts with 00:00
time_end=time_now;  % how long will propagate

[tign,fire_mask_out,fire_mask_in]=initial_tign(fire_area,time_now,time_end);
perimeter_mask=fire_mask_in & fire_mask_out;
ros_old=read_ros_from_wrfout(wrfout{num_wrf},time);
figure(2);plot_ros(long,lat,ros_old);
time_max=max(tign(:));
fprintf('propagating out from perimeter time %g to %g\n',time_now,time_max)
[t,d]=propagate_init(tign,distance);
[t,d]=propagate(t,d,1,~fire_area,fire_mask_out,distance,ros_old,time_max,1);
tign=t(:,:,2,2);
err=big(tign(perimeter_mask)-time_now); fprintf('tign change on the perimeter %g\n',err)
figure(1);mesh(long,lat,tign);drawnow
[t,d]=propagate_init(tign,distance);

for ts=(time_step*(num_wrf-1)+time):-1:2 % ts -time step
    cur_num_wrf=ceil((ts-1)/time_step);
    cur_ts=mod((ts-1),time_step);
    if (cur_ts==0) 
        cur_ts=time_step;
    end
    ros_new=read_ros_from_wrfout(wrfout{cur_num_wrf},cur_ts);
    cur_time_beg=interval*(time_step*(cur_num_wrf-1)+cur_ts);
    fprintf('propagating back in time to %g\n',cur_time_beg)
    [t,d]=propagate(t,d,-1,fire_area,fire_mask_in,distance,ros_new,cur_time_beg,1);
    tign=t(:,:,2,2);
    err=big(tign(perimeter_mask)-time_now); fprintf('tign change on the perimeter %g\n',err)
    figure(1);mesh(long,lat,tign);title(num2str(cur_time_beg));
    figure(2);plot_ros(long,lat,ros_new);
    drawnow
end
end

function distance=get_distances(long,lat)
% computing 4d array of distances between a point and its 8 neighbors
% 
% input:
%   long(i,j), lat(i,j) geographical coordinates of node [i,j], i=1:m, j=1:n, [m,n]=size(long)=size(lat)
%
% output
%   distance(i+1,j+1,a+2,b+2) = geographical distance between node [i,j] and [i+a,j+b] , a,b=-1:1
    
distance=zeros(size(long,1),size(long,2),3,3);    
i=2:size(long,1)-1;
j=2:size(long,2)-1;
for a=-1:1
   for b=-1:1
      %for j=2:size(long,2)-1
      %   for i=2:size(long,1)-1
      %      % distance between node [i,j] and [i+a,j+b]
            distance(i,j,a+2,b+2)=sqrt((long(i+a,j+b,1)-long(i,j,1)).^2+(lat(i+a,j+b,1)-lat(i,j,1)).^2);
      %   end
      %end
   end
end    
end

