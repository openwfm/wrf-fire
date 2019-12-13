function res = Mmul_v(X,trans,v)
% multiplies the flux matrix (or its transpose) and a vector
% the operation is res = op(M) * v, where op(M) = M or M^T
% set:
%    X   mesh
%    t   indicates whether the matrix is transposed or not:
%           trans = 'T' or trans = 't': op(M) = M^T
%           trans = 'N' or trans = 'n': op(M) = M
%        default is 'N'
%    v   vector to multiply

% Error checking
check_mesh(X);
if ~exist('trans','var')
    trans = 'N';
end
if (trans ~= 'T' && trans ~= 't' && trans ~= 'N' && trans ~= 'n')
    error('trans must be N, T, n, or t')
end

% Initialize result
res = zeros(size(v));

% Mesh sizes and vars
nx = size(X{1},1)-1;
ny = size(X{1},2)-1;
nz = size(X{1},3)-1;
x = X{1}; y = X{2}; z = X{3};

% This needs to be removed and replaced in code
s = cell_sizes(X);
% continue one layer up
s.dz_at_u(:,:,nz+1)=s.dz_at_u(:,:,nz);
s.dz_at_v(:,:,nz+1)=s.dz_at_v(:,:,nz);

% slope in x direction
dzdx = zeros(nx,ny+1,nz+1);
for k=1:nz+1
    for j=1:ny+1
        for i=1:nx
            dzdx(i,j,k) = ((z(i+1,j,k)-z(i,j,k))/(x(i+1,j,k)-x(i,j,k)));
        end
    end
end

% slope in y direction
dzdy = zeros(nx+1,ny,nz+1);
for k=1:nz+1
    for j=1:ny
        for i=1:nx+1
            dzdy(i,j,k) = ((z(i,j+1,k)-z(i,j,k))/(y(i,j+1,k)-y(i,j,k)));
        end
    end
end

if (trans == 't' || trans == 'T')
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% M^T * v %%%%%%%%%%%%%%%%%%%%%%%%%%%%
z_factor = (nx+1)*ny*nz + nx*(ny+1)*nz;
for k=1:nz
    for j=1:ny
        for i=1:(nx+1)
            area = s.area_u(i,j,k);
            res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) = ...
                area * v(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1));
            is_low_level = (k == 1);
            is_top_level = ((k+1) == (nz+1));
            is_first_row = (i == 1);
            is_last_row = (i == (nx + 1));
            if (is_low_level)
                % Upper level
                if (~is_last_row)
                    area_w = s.area_w(i,j,k+1);
                    dzdx_at_k = 0.5*(dzdx(i,j,k+1) + dzdx(i,j+1,k+1));
                    mul_fact = 0.5*(s.dz_at_u(i,j,k) / (s.dz_at_u(i,j,k) + s.dz_at_u(i,j,k+1)));
                    res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) = ...
                        res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) - ...
                        area_w * dzdx_at_k * mul_fact * v(z_factor + i + (j-1)*nx + k*nx*ny);
                end
                if (~is_first_row)
                    area_w = s.area_w(i-1,j,k+1);
                    dzdx_at_k = 0.5*(dzdx(i-1,j,k+1) + dzdx(i-1,j+1,k+1));
                    mul_fact = 0.5*(s.dz_at_u(i-1,j,k) / (s.dz_at_u(i-1,j,k) + s.dz_at_u(i-1,j,k+1)));
                    res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) = ...
                        res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) - ...
                        area_w * dzdx_at_k * mul_fact * v(z_factor + i - 1 + (j-1)*nx + k*nx*ny);
                end
            elseif (is_top_level)
                
                % Current level
                if (~is_last_row)
                    area_w = s.area_w(i,j,k);
                    dzdx_at_k = 0.5*(dzdx(i,j,k) + dzdx(i,j+1,k));
                    mul_fact = 0.5*(s.dz_at_u(i,j,k) / (s.dz_at_u(i,j,k) + s.dz_at_u(i,j,k-1)));
                    res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) = ...
                        res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) - ...
                        area_w * dzdx_at_k * mul_fact * v(z_factor + i + (j-1)*nx + (k-1)*nx*ny);
                end
                if (~is_first_row)
                    area_w = s.area_w(i-1,j,k);
                    dzdx_at_k = 0.5*(dzdx(i-1,j,k) + dzdx(i-1,j+1,k));
                    mul_fact = 0.5*(s.dz_at_u(i-1,j,k) / (s.dz_at_u(i-1,j,k) + s.dz_at_u(i-1,j,k-1)));
                    res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) = ...
                        res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) - ...
                        area_w * dzdx_at_k * mul_fact * v(z_factor + i - 1 + (j-1)*nx + (k-1)*nx*ny);
                end

                % Upper and current level
                if (~is_last_row)
                    area_w = s.area_w(i,j,k+1);
                    dzdx_at_k = 0.5*(dzdx(i,j,k+1) + dzdx(i,j+1,k+1));
                    mul_fact = s.dz_at_u(i,j,k) / (s.dz_at_u(i,j,k) + s.dz_at_u(i,j,k));
                    res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) = ...
                        res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) - ...
                        area_w * dzdx_at_k * mul_fact * v(z_factor + i + (j-1)*nx + k*nx*ny);
                end
                if (~is_first_row)
                    area_w = s.area_w(i-1,j,k+1);
                    dzdx_at_k = 0.5*(dzdx(i-1,j,k+1) + dzdx(i-1,j+1,k+1));
                    mul_fact = s.dz_at_u(i-1,j,k) / (s.dz_at_u(i-1,j,k) + s.dz_at_u(i-1,j,k));
                    res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) = ...
                        res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) - ...
                        area_w * dzdx_at_k * mul_fact * v(z_factor + i - 1 + (j-1)*nx + k*nx*ny);
                end
            else
                % Upper level
                if (~is_last_row)
                    area_w = s.area_w(i,j,k);
                    dzdx_at_k = 0.5*(dzdx(i,j,k) + dzdx(i,j+1,k));
                    mul_fact = 0.5*(s.dz_at_u(i,j,k) / (s.dz_at_u(i,j,k) + s.dz_at_u(i,j,k-1)));
                    res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) = ...
                        res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) - ...
                        area_w * dzdx_at_k * mul_fact * v(z_factor + i + (j-1)*nx + (k-1)*nx*ny);
                end
                if (~is_first_row)
                    area_w = s.area_w(i-1,j,k);
                    dzdx_at_k = 0.5*(dzdx(i-1,j,k) + dzdx(i-1,j+1,k));
                    mul_fact = 0.5*(s.dz_at_u(i-1,j,k) / (s.dz_at_u(i-1,j,k) + s.dz_at_u(i-1,j,k-1)));
                    res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) = ...
                        res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) - ...
                        area_w * dzdx_at_k * mul_fact * v(z_factor + i - 1 + (j-1)*nx + (k-1)*nx*ny);
                end
                
                % Current level
                if (~is_last_row)
                    area_w = s.area_w(i,j,k+1);
                    dzdx_at_k = 0.5*(dzdx(i,j,k+1) + dzdx(i,j+1,k+1));
                    mul_fact = 0.5 * s.dz_at_u(i,j,k) / (s.dz_at_u(i,j,k) + s.dz_at_u(i,j,k+1));
                    res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) = ...
                        res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) - ...
                        area_w * dzdx_at_k * mul_fact * v(z_factor + i + (j-1)*nx + k*nx*ny);
                end
                if (~is_first_row)
                    area_w = s.area_w(i-1,j,k+1);
                    dzdx_at_k = 0.5*(dzdx(i-1,j,k+1) + dzdx(i-1,j+1,k+1));
                    mul_fact = 0.5 * (s.dz_at_u(i-1,j,k) / (s.dz_at_u(i-1,j,k) + s.dz_at_u(i-1,j,k+1)));
                    res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) = ...
                        res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) - ...
                        area_w * dzdx_at_k * mul_fact * v(z_factor + i - 1 + (j-1)*nx + k*nx*ny);
                end
            end
        end
    end
end
add_factor = (nx+1)*ny*nz;
for k=1:nz
    for j=1:(ny+1)
        for i=1:nx
            area = s.area_v(i,j,k);
            res(add_factor + i + (j-1) * nx + (k-1) * (ny+1) * nx) = ...
                area * v(add_factor + i + (j-1) * nx + (k-1) * (ny+1) * nx);
            is_low_level = (k == 1);
            is_top_level = ((k+1) == (nz+1));
            is_first_col = (j == 1);
            is_last_col = (j == (ny + 1));
            if (is_low_level)
                % Upper level
                if (~is_last_col)
                    area_w = s.area_w(i,j,k+1);
                    dzdy_at_k = 0.5*(dzdy(i,j,k+1) + dzdy(i+1,j,k+1));
                    mul_fact = 0.5*(s.dz_at_v(i,j,k) / (s.dz_at_v(i,j,k) + s.dz_at_v(i,j,k+1)));
                    res(add_factor + i + (j-1) * nx + (k-1) * (ny+1) * nx) = ...
                        res(add_factor + i + (j-1) * nx + (k-1) * (ny+1) * nx) - ...
                        area_w * dzdy_at_k * mul_fact * v(z_factor + i + (j-1)*nx + k*nx*ny);
                end
                if (~is_first_col)
                    area_w = s.area_w(i,j-1,k+1);
                    dzdy_at_k = 0.5*(dzdy(i,j-1,k+1) + dzdy(i+1,j-1,k+1));
                    mul_fact = 0.5*(s.dz_at_v(i,j-1,k) / (s.dz_at_v(i,j-1,k) + s.dz_at_v(i,j-1,k+1)));
                    res(add_factor + i + (j-1) * nx + (k-1) * (ny+1) * nx) = ...
                        res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) - ...
                        area_w * dzdy_at_k * mul_fact * v(z_factor + i + (j-2)*nx + k*nx*ny);
                end
            elseif (is_top_level)
                
                % Current level
                if (~is_last_col)
                    area_w = s.area_w(i,j,k);
                    dzdy_at_k = 0.5*(dzdy(i,j,k) + dzdy(i+1,j,k));
                    mul_fact = 0.5*(s.dz_at_v(i,j,k) / (s.dz_at_v(i,j,k) + s.dz_at_v(i,j,k-1)));
                    res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) = ...
                        res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) - ...
                        area_w * dzdy_at_k * mul_fact * v(z_factor + i + (j-1)*nx + (k-1)*nx*ny);
                end
                if (~is_first_col)
                    area_w = s.area_w(i,j-1,k);
                    dzdy_at_k = 0.5*(dzdy(i,j-1,k) + dzdy(i+1,j-1,k));
                    mul_fact = 0.5*(s.dz_at_v(i,j-1,k) / (s.dz_at_v(i,j-1,k) + s.dz_at_v(i,j-1,k-1)));
                    res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) = ...
                        res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) - ...
                        area_w * dzdy_at_k * mul_fact * v(z_factor + i + (j-2)*nx + (k-1)*nx*ny);
                end

                % Cuurent and upper level
                if (~is_last_col)
                    area_w = s.area_w(i,j,k+1);
                    dzdy_at_k = 0.5*(dzdy(i,j,k+1) + dzdy(i+1,j,k+1));
                    mul_fact = s.dz_at_v(i,j,k) / (s.dz_at_v(i,j,k) + s.dz_at_v(i,j,k));
                    res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) = ...
                        res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) - ...
                        area_w * dzdy_at_k * mul_fact * v(z_factor + i + (j-1)*nx + k*nx*ny);
                end
                if (~is_first_col)
                    area_w = s.area_w(i,j-1,k+1);
                    dzdy_at_k = 0.5*(dzdy(i,j-1,k+1) + dzdy(i+1,j-1,k+1));
                    mul_fact = s.dz_at_u(i,j-1,k) / (s.dz_at_u(i,j-1,k) + s.dz_at_u(i,j-1,k));
                    res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) = ...
                        res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) - ...
                        area_w * dzdy_at_k * mul_fact * v(z_factor + i + (j-2)*nx + k*nx*ny);
                end
            else
                % Upper level
                if (~is_last_col)
                    area_w = s.area_w(i,j,k);
                    dzdy_at_k = 0.5*(dzdy(i,j,k) + dzdy(i+1,j,k));
                    mul_fact = 0.5*(s.dz_at_v(i,j,k) / (s.dz_at_v(i,j,k) + s.dz_at_v(i,j,k-1)));
                    res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) = ...
                        res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) - ...
                        area_w * dzdy_at_k * mul_fact * v(z_factor + i + (j-1)*nx + (k-1)*nx*ny);
                end
                if (~is_first_col)
                    area_w = s.area_w(i,j-1,k);
                    dzdy_at_k = 0.5*(dzdy(i,j-1,k) + dzdy(i+1,j-1,k));
                    mul_fact = 0.5*(s.dz_at_v(i,j-1,k) / (s.dz_at_v(i,j-1,k) + s.dz_at_v(i,j-1,k-1)));
                    res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) = ...
                        res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) - ...
                        area_w * dzdy_at_k * mul_fact * v(z_factor + i + (j-2)*nx + (k-1)*nx*ny);
                end
                
                % Current level
                if (~is_last_col)
                    area_w = s.area_w(i,j,k+1);
                    dzdy_at_k = 0.5*(dzdy(i,j,k+1) + dzdy(i+1,j,k+1));
                    mul_fact = 0.5 * s.dz_at_v(i,j,k) / (s.dz_at_v(i,j,k) + s.dz_at_v(i,j,k+1));
                    res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) = ...
                        res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) - ...
                        area_w * dzdy_at_k * mul_fact * v(z_factor + i + (j-1)*nx + k*nx*ny);
                end
                if (~is_first_col)
                    area_w = s.area_w(i,j-1,k+1);
                    dzdy_at_k = 0.5*(dzdy(i,j-1,k+1) + dzdy(i+1,j-1,k+1));
                    mul_fact = 0.5 * (s.dz_at_v(i,j-1,k) / (s.dz_at_v(i,j-1,k) + s.dz_at_v(i,j-1,k+1)));
                    res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) = ...
                        res(add_factor + i + (j-1) * nx + (k-1) * (ny + 1) * nx) - ...
                        area_w * dzdy_at_k * mul_fact * v(z_factor + i + (j-2)*nx + k*nx*ny);
                end
            end
        end
    end
end
add_factor = add_factor + nx*(ny+1)*nz;
for k=1:(nz+1)
    for j=1:ny
        for i=1:nx
            is_low_level = (k == 1);
            % Zero on the ground
            if (is_low_level)
                res(add_factor + i + (j-1)*nx + (k-1)*nx*ny) = 0;
            else
                res(add_factor + i + (j-1)*nx + (k-1)*nx*ny) = ...
                    s.area_w(i,j,k) * v(add_factor + i + (j-1)*nx + (k-1)*nx*ny);
            end
        end
    end
end

else % trans = 'N' || trans = 'n'
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% M * v %%%%%%%%%%%%%%%%%%%%%%%%%%%%

for k=1:nz
    for j=1:ny
        for i=1:(nx+1)
            area = s.area_u(i,j,k);
            res(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1)) = ...
                area * v(i + (j-1) * (nx+1) + (k-1) * ny * (nx+1));
        end
    end
end
add_factor = (nx+1)*ny*nz;
for k=1:nz
    for j=1:(ny+1)
        for i=1:nx
            area = s.area_v(i,j,k);
            res(add_factor + i + (j-1) * nx + (k-1) * (ny+1) * nx) = ...
                area * v(add_factor + i + (j-1) * nx + (k-1) * (ny+1) * nx);
        end
    end
end
add_factor_y = add_factor;
add_factor = add_factor + nx*(ny+1)*nz;
for k=1:(nz+1)
    for j=1:ny
        for i=1:nx
            
            is_top_level = (k == (nz+1));
            is_low_level = (k == 1);
            
            % Zero on the ground
            if (is_low_level)
                res(add_factor + i + (j-1)*nx + (k-1)*nx*ny) = 0;
            else
                area = s.area_w(i,j,k);
                dzdx_at_k = 0.5*(dzdx(i,j,k) + dzdx(i,j+1,k));
                dzdy_at_k = 0.5*(dzdy(i,j,k) + dzdy(i+1,j,k));
                if (is_top_level)
                    u_at_k = 0.5 * ((v(i + (j-1)*(nx+1) + (k-2)*(nx+1)*ny) + v(i + 1 + (j-1)*(nx+1) + (k-2)*(nx+1)*ny)) * s.dz_at_u(i,j,k) + ...
                                    (v(i + (j-1)*(nx+1) + (k-2)*(nx+1)*ny) + v(i + 1 + (j-1)*(nx+1) + (k-2)*(nx+1)*ny)) * s.dz_at_u(i,j,k-1)) / ...
                                    (s.dz_at_u(i,j,k) + s.dz_at_u(i,j,k-1));
                    v_at_k = 0.5 * ((v(add_factor_y + i + (j-1)*nx + (k-2)*nx*(ny+1)) + v(add_factor_y + i + j*nx + (k-2)*nx*(ny+1))) * s.dz_at_v(i,j,k) + ...
                                    (v(add_factor_y + i + (j-1)*nx + (k-2)*nx*(ny+1)) + v(add_factor_y + i + j*nx + (k-2)*nx*(ny+1))) * s.dz_at_v(i,j,k-1)) / ...
                                    (s.dz_at_v(i,j,k) + s.dz_at_v(i,j,k-1));
                    res(add_factor + i + (j-1)*nx + (k-1)*nx*ny) = ...
                        area * (v(add_factor + i + (j-1)*nx + (k-1)*nx*ny) - ...
                        dzdx_at_k * u_at_k - dzdy_at_k * v_at_k);
                else
                    u_at_k = 0.5 * ((v(i + (j-1)*(nx+1) + (k-1)*(nx+1)*ny) + v(i + 1 + (j-1)*(nx+1) + (k-1)*(nx+1)*ny)) * s.dz_at_u(i,j,k) + ...
                                    (v(i + (j-1)*(nx+1) + (k-2)*(nx+1)*ny) + v(i + 1 + (j-1)*(nx+1) + (k-2)*(nx+1)*ny)) * s.dz_at_u(i,j,k-1)) / ...
                                    (s.dz_at_u(i,j,k) + s.dz_at_u(i,j,k-1));
                    v_at_k = 0.5 * ((v(add_factor_y + i + (j-1)*nx + (k-1)*nx*(ny+1)) + v(add_factor_y + i + j*nx + (k-1)*nx*(ny+1))) * s.dz_at_v(i,j,k) + ...
                                    (v(add_factor_y + i + (j-1)*nx + (k-2)*nx*(ny+1)) + v(add_factor_y + i + j*nx + (k-2)*nx*(ny+1))) * s.dz_at_v(i,j,k-1)) / ...
                                    (s.dz_at_v(i,j,k) + s.dz_at_v(i,j,k-1));
                    res(add_factor + i + (j-1)*nx + (k-1)*nx*ny) = ...
                        area * (v(add_factor + i + (j-1)*nx + (k-1)*nx*ny) - ...
                        dzdx_at_k * u_at_k - dzdy_at_k * v_at_k);
                end
            end
        end
    end
end
end



