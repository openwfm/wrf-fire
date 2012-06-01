function make_moisture_input(times,t2,psfc,q2,rainc,i1,i2)
% usage: ncload moisture100.nc; 
% make_moisture_input(times,t2,psfc,q2,rainc)
%
% input: arrays from reading WRF nc files
% times: (:,k)  character string for tiime at step k
% t2(:,:,k)     surface temperature (K)
% psfc(:,:,k)   surface pressure (Pa)
% q2(:,:,k)     surface air moisture contents (kg/kg)
% rain(:,:,k)   accumulated rain (mm)
% i1,i2         indices of a point to take the values from, defaut 1,1

if ~exist('i2','var'),i2=1;end
if ~exist('i1','var'),i1=1;end

steps=size(times,2);

% convert times
ctimes=char(times');
for i=steps:-1:1
    hours(i)=24*datenum(ctimes(i,:));
end
hours=hours-hours(1);
fmt='step %i %s from start %i hours\n';
div='---------------------\n';
not_uniform=(hours(3:end)-hours(2:end-1))~=(hours(2:end-1)-hours(1:end-2));
for i=find(not_uniform)
        fprintf(fmt,i,ctimes(i,:),hours(i))
        fprintf(fmt,i+1,ctimes(i+1,:),hours(i+1))
        fprintf(fmt,i+2,ctimes(i+2,:),hours(i+2))
        fprintf(div)
end
if(any(not_uniform)),warning('Time step is not uniform'),end
not_increasing=hours(1:end-1)>=hours(2:end);
for i=find(not_increasing)
    fprintf(fmt,i,ctimes(i,:),hours(i))
    fprintf(fmt,i+1,ctimes(i+1,:),hours(i+1))
    fprintf(div)
end
if(any(not_increasing)),error('Time step must be positive'),end
end
