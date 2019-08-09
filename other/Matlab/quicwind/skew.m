function g=skew(f,h,s)
% d=skew(f,h,s)
% transform vector field on staggered skewed mesh so that flux balance
% in the cells is just difference of g on the sides
% arguments:
%    f{1}, f{2}, f{3} vector components on staggered mesh (midpoints of sides)
%    h(1:3) mesh step
%    s{1},s{2}  terrain gradient at mesh cell midpoints 
% output: 
%    u = df{1}/x1 + df{2}/dx2 + df{3}/dx3
g{1}=f{1}*(h(2)*h(3));
g{2}=f{2}*(h(1)*h(3));
g{3}=f{3}*(h(1)*h(2));
if exist('s','var'),
    % average u and v to midpoint of lower side of the cells
    % except at
    uavg = (u(1:end-1,:,1:end-2)+u(2:end,:,1:end-2)
            +u(1:end-1,:,1:end-2)+u(2:end,:,1:end-2))/4;
    dvdz = (v(:,1:end-1,3:end)+v(:,2:end,3:end)...
          -v(:,1:end-1,1:end-2)-v(:,2:end,1:end-2))/(4*h(3));
    sx = s{1};
    sy = s{2};
    for k=2:size(d,3)-1
        d(:,:,k) = d(:,:,k) + dudz .* sx + dvdz .* sy;
    end
end
