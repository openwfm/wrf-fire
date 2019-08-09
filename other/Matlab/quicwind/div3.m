function d=div3(f,h,s)
% d=div3(f,h)
% compute divergence of vector field
% arguments:
%    f{1}, f{2}, f{3} vector components on staggered mesh (midpoints of sides)
%    h(1:3) mesh step
%    s{1},s{2}  terrain gradient at mesh cell midpoints 
% output: 
%    u = df{1}/x1 + df{2}/dx2 + df{3}/dx3
u=f{1};
v=f{2};
w=f{3};
dudx = (u(2:end,:,:)-u(1:end-1,:,:))/h(1);
dvdy = (v(:,2:end,:)-v(:,1:end-1,:))/h(2);
dwdz = (w(:,:,2:end)-w(:,:,1:end-1))/h(3);
d = dudx + dvdy + dwdz;
if exist('s','var'),
    %
    dudz = (u(1:end-1,:,3:end)+u(2:end,:,3:end)...
          -u(1:end-1,:,1:end-2)-u(2:end,:,1:end-2))/(4*h(3));
    dvdz = (v(:,1:end-1,3:end)+v(:,2:end,3:end)...
          -v(:,1:end-1,1:end-2)-v(:,2:end,1:end-2))/(4*h(3));
    sx = s{1};
    sy = s{2};
    for k=2:size(d,3)-1
        d(:,:,k) = d(:,:,k) + dudz .* sx + dvdz .* sy;
    end
end

