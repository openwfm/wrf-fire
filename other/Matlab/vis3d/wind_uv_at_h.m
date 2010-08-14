function p=wind_uv_at_h(p,levels)
% p=add2struct_wind_at_h(p,levels)
% read 
% in: 
% p           structure from wrfatm2struct
% levels      1D array, heights above terrain to compute the wind
% out: additional fields in p computed, value(:,:,i) is value at heights(i)
% p.uch,p.vch         wind at cell centers (theta points, not staggered)
% p.uah,p.vah         wind at u-points, v-points (staggered)

% altitudes of the location at cell bottoms under u and v points 
[alt_bu,alt_bv]=interp_w2buv(p.alt_at_w);

% roughness of the ground under the u and v points
[z0_bu,z0_bv]=interp_w2buv(p.z0);

% log interpolation of the wind at u and v points to height levels
p.uah=log_interp_vert(p.u,alt_bu,z0_bu,levels);
p.vah=log_interp_vert(p.v,alt_bv,z0_bv,levels);

clear alt_bu alt_bv z0_bu z0_bv % free some memory

% log interpolation of the wind at center points to height
p.uch=log_interp_vert(p.uc,p.alt_at_w,p.z0,levels);
p.vch=log_interp_vert(p.vc,p.alt_at_w,p.z0,levels);

p.levels=levels;

end

function [a_bu,a_bv]=interp_w2buv(a)
% interplotate horizontally values at w points to bottom of cell 
% under the u and v points

% extend values at at w by 1 on each side by continuation just like wrf-fire
s=size1(a,4); 
alt=zeros(s(1)+2,s(2)+2,s(3),s(4)); % extend laterally by 1
alt(2:end-1,2:end-1,:,:)=a;         % embded original array
alt(1,2:end-1,:,:)=2*a(1,:,:,:)-a(2,:,:,:); % extend by reflection
alt(end,2:end-1,:,:)=2*a(end,:,:,:)-a(end-1,:,:,:);
alt(2:end-1,1,:,:)=a(:,2*1,:,:)-a(:,2,:,:); % 
alt(2:end-1,end,:,:)=2*a(:,end,:,:)-a(:,end-1,:,:);

% interpolate to bottom cell locations under u and v
a_bu=0.5*(alt(1:end-1,:,:,:)+alt(2:end,:,:,:));
a_bv=0.5*(alt(:,1:end-1,:,:)+alt(:,2:end,:,:));
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
n=length(levels);
v_levels=zeros(s(1),s(2),n,s(4));
log_levels=log(levels); % interpolate to there
for t=1:s(4)
    for i=1:s(1)
        for j=1:s(2)
            heights=[z0(i,j,t);squeeze(alt_u(i,j,:,t))-alt_bu(i,j,1,t)];
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
