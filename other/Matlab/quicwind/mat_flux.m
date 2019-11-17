function Mmat=mat_flux(X)
% M=mat_flux(X)
% Compute wind flux matrix for a hexa grid assuming all sides in the y
% direction are straight vertical while horizontal sides may be slanted
% in:
%     X{l}(i,j,k) coordinate l of node (i,j,k), if l=1,2 does not depend on k
% out:
%     Mmat        matrix computing fluxes in normal directions on mesh
% 

% Mesh computations
x = X{1}; y = X{2}; z = X{3};
% Q: this assumes that we are working on a cube based on the X direction?
[nx1, ny1, nz1] = size(x);
nx = nx1 - 1; ny = ny1 - 1; nz = nz1 - 1;
% TODO: error check to make sure we are only working in 3 dimensions?
n = nx * ny * nz1 + nx * ny1 * nz + nx1 * ny * nz;
s = cell_sizes(X);
% continue one layer up
s.dz_at_u(:,:,nz+1)=s.dz_at_u(:,:,nz);
s.dz_at_v(:,:,nz+1)=s.dz_at_v(:,:,nz);


% slope in x and y directions
dzdx = zeros(nx,ny+1,nz+1);
dzdy = zeros(nx+1,ny,nz+1);
for k=1:nz+1
    for j=1:ny+1
        for i=1:nx
            dzdx(i,j,k) = ((z(i+1,j,k)-z(i,j,k))/(x(i+1,j,k)-x(i,j,k)));
        end
    end
    for j=1:ny
        for i=1:nx+1
            dzdy(i,j,k) = ((z(i,j+1,k)-z(i,j,k))/(y(i,j+1,k)-y(i,j,k)));
        end
    end
end

% TODO: need a better estimate here...?
%   (maybe... if no terrain, then it should just be n)
nnz_est = n;
I = zeros(nnz_est,1);
J = zeros(nnz_est,1);
M = zeros(nnz_est,1);

M_col = 1;
M_row = 1;
M_entry = 1;
row_offset = nx1 * ny * nz + ny1 * nx * nz;
for ux=1:numel(X)
    
    % Size of the grid for the current direction
    grid_size = [nx,ny,nz];
    grid_size(ux) = grid_size(ux) + 1;
    X_len = grid_size(1); Y_len = grid_size(2); Z_len = grid_size(3);

    for kx=1:Z_len
        for jx=1:Y_len
            for ix=1:X_len
                if ux == 1
                    % U elements
                    M(M_entry) = s.area_u(ix,jx,kx);
                    I(M_entry) = M_row;
                    J(M_entry) = M_col;
                    M_entry = M_entry + 1;

                    % Edge cases
                    is_top_level = (kx == Z_len);
                    is_low_level = (kx == 1);
                    is_first_row = (ix == 1);
                    is_last_row = (ix == X_len);

                    if not(is_top_level)
                        if not(is_first_row)
                            M(M_entry) = -s.area_w(ix - 1, jx, kx + 1) * ...
                                0.5 * (dzdx(ix - 1, jx, kx + 1) + dzdx(ix - 1, jx + 1, kx + 1)) * ...
                                0.5 * s.dz_at_u(ix - 1, jx, kx) / (s.dz_at_u(ix - 1, jx, kx + 1) + s.dz_at_u(ix - 1, jx, kx));
                            I(M_entry) = row_offset + (ix - 1) + (jx - 1) * nx + kx * nx * ny;
                            J(M_entry) = M_col;
                            M_entry = M_entry + 1;
                        end
                        if not(is_last_row)
                            M(M_entry) = -s.area_w(ix, jx, kx + 1) * ...
                                0.5 * (dzdx(ix, jx, kx + 1) + dzdx(ix, jx + 1, kx + 1)) * ...
                                0.5 * s.dz_at_u(ix, jx, kx) / (s.dz_at_u(ix, jx, kx + 1) + s.dz_at_u(ix, jx, kx));
                            I(M_entry) = row_offset + ix + (jx - 1) * nx + kx * nx * ny;
                            J(M_entry) = M_col;
                            M_entry = M_entry + 1;
                        end

                        if not(is_low_level)
                            if not(is_first_row)
                                M(M_entry) = -s.area_w(ix - 1, jx, kx) * ...
                                    0.5 * (dzdx(ix - 1, jx, kx) + dzdx(ix - 1, jx + 1, kx)) * ...
                                    0.5 * s.dz_at_u(ix - 1, jx, kx) / (s.dz_at_u(ix - 1, jx, kx - 1) + s.dz_at_u(ix - 1, jx, kx));
                                I(M_entry) = row_offset + (ix - 1) + (jx - 1) * nx + (kx - 1) * nx * ny;
                                J(M_entry) = M_col;
                                M_entry = M_entry + 1;
                            end
                            if not(is_last_row)
                                M(M_entry) = -s.area_w(ix, jx, kx) * ...
                                    0.5 * (dzdx(ix, jx, kx) + dzdx(ix, jx + 1, kx)) * ...
                                    0.5 * s.dz_at_u(ix, jx, kx) / (s.dz_at_u(ix, jx, kx - 1) + s.dz_at_u(ix, jx, kx));
                                I(M_entry) = row_offset + ix + (jx - 1) * nx + (kx - 1) * nx * ny;
                                J(M_entry) = M_col;
                                M_entry = M_entry + 1;
                            end
                        end
                    else
                        if not(is_first_row)
                            M(M_entry) = -s.area_w(ix - 1, jx, kx) * ...
                                0.5 * (dzdx(ix - 1, jx, kx) + dzdx(ix - 1, jx + 1, kx)) * ...
                                0.5 * s.dz_at_u(ix - 1, jx, kx) / (s.dz_at_u(ix - 1, jx, kx) + s.dz_at_u(ix - 1, jx, kx - 1));
                            I(M_entry) = row_offset + (ix - 1) + (jx - 1) * nx + (kx - 1) * nx * ny;
                            J(M_entry) = M_col;
                            M_entry = M_entry + 1;
                            M(M_entry) = -s.area_w(ix - 1, jx, kx + 1) * ...
                                0.25 * (dzdx(ix - 1, jx, kx + 1) + dzdx(ix - 1, jx + 1, kx + 1));
                            I(M_entry) = row_offset + (ix - 1) + (jx - 1) * nx + kx * nx * ny;
                            J(M_entry) = M_col;
                            M_entry = M_entry + 1;
                        end
                        if not(is_last_row)
                            M(M_entry) = -s.area_w(ix, jx, kx) * ...
                                0.5 * (dzdx(ix, jx, kx) + dzdx(ix, jx + 1, kx)) * ...
                                0.5 * (s.dz_at_u(ix, jx, kx) / (s.dz_at_u(ix, jx, kx - 1) + s.dz_at_u(ix, jx, kx)));
                            I(M_entry) = row_offset + ix + (jx - 1) * nx + (kx - 1) * nx * ny;
                            J(M_entry) = M_col;
                            M_entry = M_entry + 1;
                            M(M_entry) = -s.area_w(ix, jx, kx + 1) * ...
                                0.25 * (dzdx(ix, jx, kx + 1) + dzdx(ix, jx + 1, kx + 1));
                            I(M_entry) = row_offset + ix + (jx - 1) * nx + kx * nx * ny;
                            J(M_entry) = M_col;
                            M_entry = M_entry + 1;
                        end
                    end
                elseif ux == 2
                    % V elements
                    M(M_entry) = s.area_v(ix,jx,kx);
                    I(M_entry) = M_row;
                    J(M_entry) = M_col;
                    M_entry = M_entry + 1;
                    
                    % Edge cases
                    is_top_level = (kx == Z_len);
                    is_low_level = (kx == 1);
                    is_first_row = (jx == 1);
                    is_last_row = (jx == Y_len);
                    
                    if not(is_top_level)
                        if not(is_first_row)
                            M(M_entry) = -s.area_w(ix, jx - 1, kx + 1) * ...
                                0.5 * (dzdy(ix, jx - 1, kx + 1) + dzdy(ix + 1, jx - 1, kx + 1)) * ...
                                0.5 * s.dz_at_v(ix, jx - 1, kx) / (s.dz_at_v(ix, jx - 1, kx + 1) + s.dz_at_v(ix, jx - 1, kx));
                            I(M_entry) = row_offset + ix + (jx - 2) * nx + kx * nx * ny;
                            J(M_entry) = M_col;
                            M_entry = M_entry + 1;
                        end
                        if not(is_last_row)
                            M(M_entry) = -s.area_w(ix, jx, kx + 1) * ...
                                0.5 * (dzdy(ix, jx, kx + 1) + dzdy(ix + 1, jx, kx + 1)) * ...
                                0.5 * s.dz_at_v(ix, jx, kx) / (s.dz_at_v(ix, jx, kx + 1) + s.dz_at_v(ix, jx, kx));
                            I(M_entry) = row_offset + ix + (jx - 1) * nx + kx * nx * ny;
                            J(M_entry) = M_col;
                            M_entry = M_entry + 1;
                        end

                        if not(is_low_level)
                            if not(is_first_row)
                                M(M_entry) = -s.area_w(ix, jx - 1, kx) * ...
                                    0.5 * (dzdy(ix, jx - 1, kx) + dzdy(ix + 1, jx - 1, kx)) * ...
                                    0.5 * s.dz_at_v(ix, jx - 1, kx) / (s.dz_at_v(ix, jx - 1, kx - 1) + s.dz_at_v(ix, jx - 1, kx));
                                I(M_entry) = row_offset + ix + (jx - 2) * nx + (kx - 1) * nx * ny;
                                J(M_entry) = M_col;
                                M_entry = M_entry + 1;
                            end
                            if not(is_last_row)
                                M(M_entry) = -s.area_w(ix, jx, kx) * ...
                                    0.5 * (dzdy(ix, jx, kx) + dzdy(ix + 1, jx, kx)) * ...
                                    0.5 * s.dz_at_v(ix, jx, kx) / (s.dz_at_v(ix, jx, kx - 1) + s.dz_at_v(ix, jx, kx));
                                I(M_entry) = row_offset + ix + (jx - 1) * nx + (kx - 1) * nx * ny;
                                J(M_entry) = M_col;
                                M_entry = M_entry + 1;
                            end
                        end
                    else
                        if not(is_first_row)
                            M(M_entry) = -s.area_w(ix, jx - 1, kx) * ...
                                0.5 * (dzdy(ix, jx - 1, kx) + dzdy(ix + 1, jx - 1, kx)) * ...
                                0.5 * s.dz_at_v(ix, jx - 1, kx) / (s.dz_at_v(ix, jx - 1, kx) + s.dz_at_v(ix, jx - 1, kx - 1));
                            I(M_entry) = row_offset + ix + (jx - 2) * nx + (kx - 1) * nx * ny;
                            J(M_entry) = M_col;
                            M_entry = M_entry + 1;
                            
                            M(M_entry) = -s.area_w(ix, jx - 1, kx + 1) * ...
                                0.25 * (dzdy(ix, jx - 1, kx + 1) + dzdy(ix + 1, jx - 1, kx + 1));
                            I(M_entry) = row_offset + ix + (jx - 2) * nx + kx * nx * ny;
                            J(M_entry) = M_col;
                            M_entry = M_entry + 1;
                        end
                        if not(is_last_row)
                            M(M_entry) = -s.area_w(ix, jx, kx) * ...
                                0.5 * (dzdy(ix, jx, kx) + dzdy(ix + 1, jx, kx)) * ...
                                0.5 * (s.dz_at_v(ix, jx, kx) / (s.dz_at_v(ix, jx, kx - 1) + s.dz_at_v(ix, jx, kx)));
                            I(M_entry) = row_offset + ix + (jx - 1) * nx + (kx - 1) * nx * ny;
                            J(M_entry) = M_col;
                            M_entry = M_entry + 1;
                            
                            M(M_entry) = -s.area_w(ix, jx, kx + 1) * ...
                                0.25 * (dzdy(ix, jx, kx + 1) + dzdy(ix + 1, jx, kx + 1));
                            I(M_entry) = row_offset + ix + (jx - 1) * nx + kx * nx * ny;
                            J(M_entry) = M_col;
                            M_entry = M_entry + 1;
                        end
                    end
                elseif ux == 3 && kx ~= 1
                    % W elements
                    M(M_entry) = s.area_w(ix, jx, kx);
                    I(M_entry) = M_row;
                    J(M_entry) = M_col;
                    M_entry = M_entry + 1;
                end
            M_col = M_col + 1;
            M_row = M_row + 1;
            end
        end
    end
end
Mmat = sparse(I,J,M,n,n);
