function tign=propagate(tign,dir,fire_area,distance,ros,time_now)
% input:
%   tign - ignition time, matrix size (m,n).
%          Points not burning should be > t_now (dir=1) or < t_now (dir=-1)
%   dir  - 1 forward, -1 backward
%   fire_area - 1 what can burn, 0 what not
%   distance(m,n,3,3) - distance
%   t_now - the latest time to propagate to (earliest if dir=-1)
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

m=size(tign,1);n=size(tign,2);
t=zeros(m,n,3,3);
for i=1:m, for j=1:n
        t(i,j,:,:)=tign(i,j);
end,end
print=1;
d=distance;
active=squeeze(t(:,:,2,2)<time_now);
for step=1:100,
    t_old=t;
    for i=1:m, for j=1:n,
        if active(i,j) & fire_area(i,j)
            for a=1:3, for b=1:3, if a ~=2 | b~=2,
                if print,fprintf('step %i point %i %i direction %i %i time %g ',step,i,j,a-2,b-2,t(i,j,a,b));end
                dt = max(time_now-t(i,j,a,b),0);    % time available to propagate
                if dt>0,
                    dd = dt.*ros(i,j,a,b);        % distance traveled to tnow
                    if d(i,j,a,b)> dd,              % positive distance remains
                        t(i,j,a,b)=time_now;
                        d(i,j,a,b)= d(i,j,a,b)-dd;  % decrease the distances remaining
                        if print,fprintf('distance remaining %g time %g',d(i,j,a,b),time_now);end
                    elseif d(i,j,a,b)>0,
                        t_end=t(i,j,a,b)+d(i,j,a,b)./ros(i,j,a,b); % time at the end point
                        if print,fprintf('time at end %g ',t_end);end
                        ii=i+a-2; % the grid point this end point coincides with 
                        jj=j+b-2;
                        if ii>=1 & ii<=m,if jj>=1 & jj<=n & fire_area(ii,jj)
                            val=min(t(ii,jj,2,2),t_end);
                            if print,fprintf('setting %i %i from %g to %g',ii,jj,t(ii,jj,2,2),val);end
                            t(ii,jj,:,:)=val;               % reinitialize the point ii, jj
                            d(ii,jj,:,:)=distance(ii,jj,:,:); % no distance traveled 
                            active(ii,jj)=true;             % can propagate from this
                        end, end
                        t(i,j,a,b)=t_end;
                        d(i,j,a,b)=0;
                    end
                end
                if print,fprintf('\n');end
            end, end, end
        end
    end, end
    step,tign=t(:,:,2,2)
    done=~any(t(:)-t_old(:));
    if done,
        break
    end
    % fclose(h)
end
tign=t(:,:,2,2);

