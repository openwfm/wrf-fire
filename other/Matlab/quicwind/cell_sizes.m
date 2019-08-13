function s=cell_sizes(X)
% s=cell_sizes(X) 
% compute averaged sizes and areas of cell sizes for a deformed mesh
% 
% input:
%   X   cell array, nodal coordinatees x y z
% output
%   s structure contains:
%    dz_at_u same size as u
%    dy_at_u
%     area_u
%    dz_at_v same size as v
%    dx_at_v
%     area_v
%    dx_at_w same size as w
%    dy_at_w
%     area_w

x = X{1}; y = X{2}; z = X{3};
[nx1,ny1,nz1] = size(x);
nx = nx1-1; ny = ny1-1; nz = nz1-1;

if any(size(y)~=[nx1,ny1,nz1])|any(size(z)~=[nx1,ny1,nz1]),
    error('cell_sizes: arrays x y z must be the same size')
end
if ~all(all((x(2:end,:,:)>x(1:end-1,:,:)))),
    error('cell_sizes: x increments in array x must be positive')
end
if ~all(all((y(:,2:end,:)>y(:,1:end-1,:)))),
    error('cell_sizes: y increments in array y must be positive')
end
if ~all(all((y(:,2:end,:)>y(:,1:end-1,:)))),
    error('cell_sizes: z increments in array z must be positive')
end

%                                             ^ z,w,k
%     (i,j+1,k+1)---------(i+1,j+1,k+1        |
%     /  |               / |                  |    /
%    /   |              /  |                  |   / y,v,j
%   /    |             /   |                  |  /
%(i,j,k+1)----------(i+1,j,k+1)               | /
%  |    /|            |    |                  |--------> x,u,i
%  |  dy |            |    |
%  | /   |            |    |
%  |/    |            |    |
%  |   (i,j+1,k)---------(i+1,j+1,k)
%  |   /              |  /
%  |  /               | /
%  | /                |/
%(i,j,k)----------(i+1,j,k)


s.dz_at_u = zeros(nx+1,ny,nz);  % dz for u etc.
s.dy_at_u = zeros(nx+1,ny,nz);  % dz for u etc.
s.area_u  = zeros(nx+1,ny,nz);  % dz for u etc.
for k=1:nz
    for j=1:ny
        for i=1:nx+1
            s.dz_at_u(i,j,k) = 0.5*(z(i,j,k+1)-z(i,j,k)+z(i,j+1,k+1)-z(i,j+1,k));
            s.dy_at_u(i,j,k) = 0.5*(y(i,j+1,k)-y(i,j,k)+y(i,j+1,k+1)-y(i,j,k+1));
            s.area_u (i,j,k) = s.dy_at_u(i,j,k)*s.dz_at_u(i,j,k);
        end
    end
end


s.dz_at_v = zeros(nx,ny+1,nz);  
s.dx_at_v = zeros(nx,ny+1,nz);
s.area_v  = zeros(nx,ny+1,nz);
for k=1:nz
    for j=1:ny+1
        for i=1:nx
            s.dz_at_v(i,j,k) = 0.5*(z(i,j,k+1)-z(i,j,k)+z(i+1,j,k+1)-z(i+1,j,k));
            s.dx_at_v(i,j,k) = 0.5*(x(i+1,j,k)-x(i,j,k)+x(i+1,j,k+1)-x(i,j,k+1));
            s.area_v(i,j,k)=s.dx_at_v(i,j,k)*s.dz_at_v(i,j,k);
        end
    end
end

s.dx_at_w = zeros(nx,ny,nz+1);
s.dy_at_w = zeros(nx,ny,nz+1);
s.area_w = zeros(nx,ny,nz+1);
for k=1:nz+1   
    for j=1:ny
        for i=1:nx
            s.dx_at_w(i,j,k) = 0.5*(x(i+1,j,k)-x(i,j,k)+x(i+1,j,k)-x(i,j+1,k));
            s.dy_at_w(i,j,k) = 0.5*(y(i,j+1,k)-y(i,j,k)+y(i+1,j+1,k)-y(i+1,j,k));
            s.area_w(i,j,k) = s.dx_at_w(i,j,k) *  s.dy_at_w(i,j,k);
        end
    end
end

end