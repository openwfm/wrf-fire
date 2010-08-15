function p=wind_uv_at_h(p,heights)
% p=add2struct_wind_at_h(p,heights,levels)
% read 
% in: 
% p           structure from wrfatm2struct
% heights     1D array, heights above terrain to compute the wind
% out: additional fields in p computed, value(:,:,i) is value at heights(i)
% p.uch,p.vch         wind at cell centers (theta points, not staggered)
% p.uah,p.vah         wind at u-points, v-points (staggered)

if any(p.z0(:)<=0),
    error('roughness height z0 must be positive')
end

% altitudes of the location at cell bottoms under u and v points 
[alt_bu,alt_bv]=interp_w2buv(p.alt_at_w);

% roughness of the ground under the u and v points
[z0_bu,z0_bv]=interp_w2buv(p.z0);

% log interpolation of the wind at u and v points to height
p.uah=log_interp_vert(p.u,alt_bu,z0_bu,heights);
p.vah=log_interp_vert(p.v,alt_bv,z0_bv,heights);

clear alt_bu alt_bv z0_bu z0_bv % free some memory

% log interpolation of the wind at center points to height
p.uch=log_interp_vert(p.uc,p.alt_at_w,p.z0,heights);
p.vch=log_interp_vert(p.vc,p.alt_at_w,p.z0,heights);

p.heights=heights;

end

function [a_bu,a_bv]=interp_w2buv(a)
% interpolate horizontally values at w points to bottom of cell 
% under the u and v points

% extend values at at w by 1 on each side by continuation
etype='constant'; % extend by constant
s=size1(a,4); 
alt=zeros(s(1)+2,s(2)+2,s(3),s(4)); % extend laterally by 1
alt(2:end-1,2:end-1,:,:)=a;         % embded original array
alt(1,2:end-1,:,:)=extend(a(1,:,:,:),a(2,:,:,:),etype); % extend by reflection
alt(end,2:end-1,:,:)=extend(a(end,:,:,:),a(end-1,:,:,:),etype);
alt(2:end-1,1,:,:)=extend(a(:,2*1,:,:),a(:,2,:,:),etype); % 
alt(2:end-1,end,:,:)=extend(a(:,end,:,:),a(:,end-1,:,:),etype);

% interpolate to bottom cell locations under u and v
a_bu=0.5*(alt(1:end-1,:,:,:)+alt(2:end,:,:,:));
a_bv=0.5*(alt(:,1:end-1,:,:)+alt(:,2:end,:,:));
end

function x0=extend(x1,x2,etype)
switch etype(1)
    case 'c'  
        x0=x1;  % constant
    case 'r'
        x0=x1+(x1-x2); % reflection
    otherwise
        type
        error('extend: unknown type')
end
end

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

function s=size1(a,ndim)
% s=size1(a,ndim)
% return size(a) extended by 1 to ndim dimensions
s=size(a);
s=[s,ones(1,ndim-length(s))];
end 
