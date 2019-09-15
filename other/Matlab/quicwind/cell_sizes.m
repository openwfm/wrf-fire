function s=cell_sizes(X)
% s=cell_sizes(X) 
% compute averaged sizes and areas of cell sizes for a deformed mesh
% 
% input:
%   X   cell array, nodal coordinatees x y z
% output
%   s structure contains arrays 
% same shape as u, v, w:
%    dz_at_u   averaged increment in z on u-sides etc
%    dy_at_u
%     area_u   area of u-side 
%    dz_at_v   etc
%    dx_at_v
%     area_v
%    dx_at_w 
%    dy_at_w
%     area_w
%   weight_u
%   weight_v
%   weight_w
%  same shape as x,y,z:
%    depth_x  increment in x from x side to next
%    depth_y  etc
%    depth_z

check_mesh(X);

x = X{1}; y = X{2}; z = X{3};
[nx1,ny1,nz1] = size(x);
nx = nx1-1; ny = ny1-1; nz = nz1-1;


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

% cell dimensions - distances between midpoints
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

% averaged depth of cells in the 3 directions
s.depth_x=zeros(nx,ny,nz);
s.depth_y=zeros(nx,ny,nz);
s.depth_z=zeros(nx,ny,nz);
for l1=0:1
    for l2=0:1
        for k=1:nz
            for j=1:ny
                for i=1:nx
                    s.depth_x(i,j,k)=s.depth_x(i,j,k)+0.25*(x(i+1,j+l1,k+l2)-x(i,j+l1,k+l2));
                    s.depth_y(i,j,k)=s.depth_y(i,j,k)+0.25*(y(i+l1,j+1,k+l2)-y(i+l1,j,k+l2));
                    s.depth_z(i,j,k)=s.depth_z(i,j,k)+0.25*(z(i+l1,j+l2,k+1)-z(i+l1,j+l2,k));
                end
            end
        end
    end
end

s.weight_u=zeros(nx+1,ny,nz);
s.weight_v=zeros(nx,ny+1,nz);
s.weight_w=zeros(nx,ny,nz+1);
for k=1:nz   
    for j=1:ny
        for i=1:nx
            s.weight_u(i,j,k)=0.5*s.depth_x(i,j,k)*(s.area_u(i+1,j,k)+s.area_u(i,j,k));
        end
        i=nx+1;
        s.weight_u(i,j,k)=s.depth_x(i-1,j,k)*s.area_u(i,j,k);
    end
end
for k=1:nz   
    for i=1:nx
        for j=1:ny
            s.weight_v(i,j,k)=0.5*s.depth_y(i,j,k)*(s.area_v(i,j+1,k)+s.area_v(i,j,k));
        end
        j=ny+1;
        s.weight_v(i,j,k)=s.depth_y(i,j-1,k)*s.area_v(i,j,k);
    end
end
for i=1:nx
   for j=1:ny
       for k=1:nz   
            s.weight_w(i,j,k)=0.5*s.depth_z(i,j,k)*(s.area_w(i,j,k+1)+s.area_w(i,j,k));
       end
       k=nz+1;
       s.weight_w(i,j,k)=s.depth_z(i,j,k-1)*s.area_w(i,j,k);
   end
end


end