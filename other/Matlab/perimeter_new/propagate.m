%function propagate
% state encoding
%   t(i,j,a,b)  fire arrival time at a point connecting (i,j) and (i+a-1,j+b-1)
%   d(i,j,a,b)  distance remaining to (i+a-1,j+b-1), scaled between 0 and 1
% data
%   r(i,j,a,b)  rate of spread along the line connecting (i,j) and (i+a-1,j+b-1)

% start: fireline all at tnow, progressing to tnow

% to start: 
%   t(i,j,:,:)=fire arrival time already burning
%   

d_orig=d;
m=size(t,1);n=size(t,2);
active=squeeze(t(:,:,2,2)<t_now);
for step=1:100,
    t_old=t;
    for i=1:m, for j=1:n,
        if active(i,j)
            for a=1:3, for b=1:3, if a ~=2 | b~=2,
                fprintf('step %i point %i %i direction %i %i time %g ',step,i,j,a-2,b-2,t(i,j,a,b))
                dt = max(t_now-t(i,j,a,b),0);    % time available to propagate
                if dt>0,
                    dd = dt.*ros(i,j,a,b);        % distance traveled to tnow
                    if d(i,j,a,b)> dd,              % positive distance remains
                        t(i,j,a,b)=t_now;
                        d(i,j,a,b)= d(i,j,a,b)-dd;  % decrease the distances remaining
                        fprintf('distance remaining %g time %g',d(i,j,a,b),t_now);
                    elseif d(i,j,a,b)>0,
                        t_end=t(i,j,a,b)+d(i,j,a,b)./ros(i,j,a,b); % time at the end point
                        fprintf('time at end %g ',t_end)
                        ii=i+a-2; % the grid point this end point coincides with 
                        jj=j+b-2;
                        if ii>=1 & ii<=m,if jj>=1 & jj<=n,
                            val=min(t(ii,jj,2,2),t_end);
                            fprintf('setting %i %i from %g to %g',ii,jj,t(ii,jj,2,2),val);
                            t(ii,jj,:,:)=val;    % reinitialize the point ii, jj
                            d(ii,jj,:,:)=d_orig(ii,jj,:,:);
                            active(ii,jj)=true;
                        end, end
                        t(i,j,a,b)=t_end;
                        d(i,j,a,b)=0;
                    end
                end
                fprintf('\n')
            end, end, end
        end
    end, end
    done=~any(t(:)-t_old(:));
    if done,
        break
    end
    % fclose(h)
end
