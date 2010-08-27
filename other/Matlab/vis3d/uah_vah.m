% function [uah,vah]=u_v_at_h(z0,u,v,altw,heights)
% [uah,vah]=u_v_at_h(z0,u,v,altw,heights)
% interpolate wind on staggered grid to given
%
% in:
% z0        2D roughness length at cell centers (under w points) 
% u         3D wind, staggered in x direction
% v         3D wind, staggered in y direction
% altw      3D altitude, at w points
% heights   1D array of heights above the terrain to interpolate to
%
% out:
% uah       3D uah(:,:,i) is u interpolated to heights(i)
% vah       3D vah(:,:,i) is v interpolated to heights(i)

% staggered dimension x
%                u(1)---z0(1)---u(2)---z0(2)--- ...--u(n)---z0(n)---u(n+1)
% interpolated:                z0u(2)    ...        z0u(n)

% interpolate z0 to under u and v points
z0u = 0.5*(z0(1:end-1,:)+z0(2:end,:));
z0v = 0.5*(z0(:,1:end-1)+z0(:,2:end));

% interpolate altitude to under u and v points 
altub=0.5*(altw(1:end-1,:,:)+altw(2:end,:,:));
altvb=0.5*(altw(:,1:end-1,:)+altw(:,2:end,:));

% interpolate altitude vertically to u and v points
hgtu=0.5*(altub(:,:,1:end-1)+altub(:,:,2:end));
hgtv=0.5*(altvb(:,:,1:end-1)+altvb(:,:,2:end));

% subtract the altitude of the ground to get height (above the ground)
for k=1:size(hgtu,3)
    hgtu(:,:,k)=hgtu(:,:,k)-altub(:,:,1);
    hgtv(:,:,k)=hgtv(:,:,k)-altvb(:,:,1);
end

% log interpolation of the wind at u and v points to height
ua=log_interp_vert(u(2:end-1,:,:),hgtu,z0u,height);
va=log_interp_vert(v(:,2:end-1,:),hgtv,z0v,height);

% extend by extrapolation in staggered dimension
for i=length(height(:)):-1:1
    h=height(i);
    uah(:,:,i)=[2*ua(1,:,i)-ua(2,:,i);ua(:,:,i);2*ua(end,:,i)-ua(end-1,:,i)];
    vah(:,:,i)=[2*va(:,1,i)-va(:,2,i),va(:,:,i),2*va(:,end,i)-va(:,end-1,i)];
end

% end
