function v_levels=log_interp_vert(u,hgtu,z0,levels)
% vertical log interpolation
% u         values given at u poits (half eta levels)
% hgtu      heights at u points 
% z0        roughness height 
% levels    heights to interpolate to (3rd index)
% Note: the computation runs over all i,j (dimensions 1 and 2 in u) 
% and all timesteps (dimensions 4)

% extend u by zeros at the ground
s=size1(u,4);
u0=zeros(s(1),s(2),s(3)+1,s(4));
u0(:,:,2:end,:)=u;

levels=levels(:);
if any(levels<=0),
    disp(levels)
    error('levels must be positive for log interpolation')
end
log_levels=log(levels); % interpolate to there
n=length(levels);
v_levels=zeros(s(1),s(2),n,s(4));
for t=1:s(4)
    for i=1:s(1)
        for j=1:s(2)
            heights=[z0(i,j,t);squeeze(hgtu(i,j,:,t))];
            if heights(2)<=heights(1),
                disp(heights)
                error('first level must be higher than z0')
            end
            if any(heights<=0),
                disp(heights)
                error('heights must be positive for log interpolation')
            end
            log_heights=log(heights);
            u_ijt=squeeze(u0(i,j,:,t));
            v_levels(i,j,:,t)=interp1(log_heights,u_ijt,log_levels);
        end
    end
end
end