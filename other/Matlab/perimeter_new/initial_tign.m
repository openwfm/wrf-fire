function [tign,fire_mask_out,fire_mask_in]=initial_tign(fire_area,time_now,max_time)
% in: 
%   fire_area  1 inside and on perimeter
%   time       the time of the perimeter 
%   dir        1 going outside, -1 inside
% out:
%   tign=time_now on perimeter (in fire area, with neighbor who is not)
%       =time_now+max_time outside fire_area
%       =time_now-max_time inside fire_area
%   fire_mask_out 
%   fire_mask_in

[m,n]=size(fire_area);
fire_area=fire_area>0;
tign=zeros(m,n);
tign(fire_area)=time_now-max_time;
tign(~fire_area)=time_now+max_time;
fire_area_ext=true(m+2,n+2);
fire_area_ext(2:m+1,2:n+1)=fire_area;
fire_mask_in=fire_area;
fire_mask_out=~fire_area;
for i=1:m
    for j=1:n        
        % neigh=fire_area_ext(i:i+2,j:j+2);
        if fire_area(i,j) & ~(...
                fire_area_ext(i+1,j) & fire_area_ext(i+1,j+2) & ...
                fire_area_ext(i,j+1) & fire_area_ext(i+2,j+1) )
                tign(i,j)=time_now;
                fire_mask_out(i,j)=true;
                fire_mask_in(i,j)=true;
        end
    end
end
end
