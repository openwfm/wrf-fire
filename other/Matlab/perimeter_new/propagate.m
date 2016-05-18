function tign=propagate(tign,dir,fire_area,distance,ros,time_now,print)
% input:
%   tign - ignition time, matrix size (m,n).
%          Points not burning should be > t_now (dir=1) or < t_now (dir=-1)
%   dir  - 1 forward, -1 backward
%   fire_area - 1 what can burn, 0 what not
%   distance(m,n,3,3) - distance
%   time_now - the latest time to propagate to (earliest if dir=-1)
%   print  1 for tracing
% output:
%   tign - fire arrival time up to t_now

% state encoding
%   t(i,j,a,b)  fire arrival time at a point connecting (i,j) and (i+a-1,j+b-1)
%   d(i,j,a,b)  distance remaining to (i+a-1,j+b-1), scaled between 0 and 1
% data
%   r(i,j,a,b)  rate of spread along the line connecting (i,j) and (i+a-1,j+b-1)

% start: fireline all at tnow, progressing to tnow

% to start: 
%   t(i,j,:,:)=fire arrival time already burning
%   

if abs(dir) ~= 1, error('dir must be +-1'),end 
m=size(tign,1);n=size(tign,2);
t=zeros(m,n,3,3);
for i=1:m, for j=1:n
        t(i,j,:,:)=tign(i,j);
end,end
if ~exist('print','var'),
    print=0;
end
d=distance;
active=squeeze(dir*(time_now-t(:,:,2,2))>0);
for step=1:100,
    t_old=t;
    for i=1:m, for j=1:n,
        if active(i,j) & fire_area(i,j)
            for a=1:3, for b=1:3, if a ~=2 | b~=2,
                if print>1,fprintf('step %i point %i %i direction %i %i time %g ',step,i,j,a-2,b-2,t(i,j,a,b));end
                dt = max(dir*(time_now-t(i,j,a,b)),0);    % time available to propagate
                if dt>0,
                    dd = dt.*ros(i,j,a,b);        % distance traveled to tnow
                    if d(i,j,a,b)> dd,              % positive distance remains
                        t(i,j,a,b)=time_now;
                        d(i,j,a,b)= d(i,j,a,b)-dd;  % decrease the distances remaining
                        if print>1,fprintf('distance remaining %g time %g',d(i,j,a,b),t(i,j,a,b));end
                    elseif d(i,j,a,b)>0,
                        t_end=t(i,j,a,b)+dir*d(i,j,a,b)./ros(i,j,a,b); % time at the end point
                        if print>1,fprintf('time at end %g ',t_end);end
                        ii=i+a-2; % the grid point this end point coincides with 
                        jj=j+b-2;
                        if ii>=1 & ii<=m,if jj>=1 & jj<=n & fire_area(ii,jj)
                            val=dir*min(dir*t(ii,jj,2,2),dir*t_end);
                            if print>1,fprintf('setting %i %i from %g to %g',ii,jj,t(ii,jj,2,2),val);end
                            t(ii,jj,:,:)=val;               % reinitialize the point ii, jj
                            d(ii,jj,:,:)=distance(ii,jj,:,:); % no distance traveled 
                            active(ii,jj)=true;             % can propagate from this
                        end, end
                        t(i,j,a,b)=t_end;
                        d(i,j,a,b)=0;
                    end
                end
                if print>1,fprintf('\n');end
            end, end, end
        end
    end, end
    if print>0,step,tign=t(:,:,2,2),end
    done=~any(t(:)-t_old(:));
    if done,
        break
    end
end
tign=t(:,:,2,2);

