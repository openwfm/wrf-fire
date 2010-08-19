function v_levels=log_interp_vert(u,alt_bu,z0,levels)
% vertical log interpolation
% u   values given at u poits (half eta levels)
% z0  roughtness height 
% alt_at_w

% extend u by zeros at the ground
s=size1(u,4);
u0=zeros(s(1),s(2),s(3)+1,s(4));
u0(:,:,2:end,:)=u;

% find altitude at u (half eta levels)
alt_u=0.5*(alt_bu(:,:,1:end-1,:)+alt_bu(:,:,2:end,:));

levels=levels(:);
if any(levels<=0),
    levels
    error('levels must be positive for log interpolation')
end
log_levels=log(levels); % interpolate to there
n=length(levels);
v_levels=zeros(s(1),s(2),n,s(4));
for t=1:s(4)
    for i=1:s(1)
        for j=1:s(2)
            heights=[z0(i,j,t);squeeze(alt_u(i,j,:,t))-alt_bu(i,j,1,t)];
            if any(heights<=0),
                heights
                error('heights must be positive for log interpolation')
            end
            log_heights=log(heights);
            u_ijt=squeeze(u0(i,j,:,t));
            v_levels(i,j,:,t)=interp1(log_heights,u_ijt,log_levels);
        end
    end
end
end