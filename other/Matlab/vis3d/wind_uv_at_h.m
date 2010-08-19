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

% log interpolation of the wind at center points to height
p.uch=log_interp_vert(p.uc,p.alt_at_w,p.z0,heights);
p.vch=log_interp_vert(p.vc,p.alt_at_w,p.z0,heights);

p.heights=heights;

end
function s=size1(a,ndim)
% s=size1(a,ndim)
% return size(a) extended by 1 to ndim dimensions
s=size(a);
s=[s,ones(1,ndim-length(s))];
end 
