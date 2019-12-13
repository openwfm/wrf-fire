function res = Dmul_v(X,trans,v)
% multiplies the divergence matrix (or its transpose) and a vector
% the operation is res = op(D) * v, where op(D) = D or D^T
% set:
%    X       mesh
%    trans   indicates whether the matrix is transposed or not:
%               trans = 'T' or trans = 't': op(D) = D^T
%               trans = 'N' or trans = 'n': op(D) = D
%            default is 'N'
%    v       vector to multiply

% Error checking
check_mesh(X);
if ~exist('trans','var')
    trans = 'N';
end
if (trans ~= 'T' && trans ~= 't' && trans ~= 'N' && trans ~= 'n')
    error('trans must be N, T, n, or t')
end

% Mesh sizes and vars
nx = size(X{1},1)-1;
ny = size(X{1},2)-1;
nz = size(X{1},3)-1;
x = X{1}; y = X{2}; z = X{3};

% Initialize result
if (trans == 'T' || trans == 't')
    res = zeros((nx+1)*ny*nz+nx*(ny+1)*nz+nx*ny*(nz+1),1);
else
    res = zeros(nx*ny*nz,1);
end

if (trans == 't' || trans == 'T')
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% D^T * v %%%%%%%%%%%%%%%%%%%%%%%%%%%%

helper_x = 1;
helper_y = 1;
helper_z = 1;
for k=1:nz
    for j=1:ny
        for i=1:(nx+1)
            if (i == 1)
                res(ny*(nx+1)*(k-1)+(nx+1)*(j-1)+1) = ...
                    -v(nx * (helper_x - 1) + 1);
            elseif (i == (nx+1))
                res(ny*(nx+1)*(k-1)+(nx+1)*(j-1)+nx+1) = ...
                    v(nx * helper_x);
            else
                res(ny*(nx+1)*(k-1)+(nx+1)*(j-1)+i) = ...
                    v(nx * (helper_x-1) + i-1)-v(nx * (helper_x-1)+i);
            end
        end
        helper_x = helper_x + 1;
    end
end
add_factor = (nx+1)*ny*nz;
for k=1:nz
    for j=1:(ny+1)
        for i=1:nx
            if (j == 1)
                res(add_factor + (ny+1)*nx*(k-1)+nx*(j-1)+i) = ...
                    -v(helper_y - 1 + i);
            elseif (j == (ny+1))
                res(add_factor + (ny+1) * nx * k - nx + i) = ...
                    v(helper_y);
                helper_y = helper_y + 1;
            else
                res(add_factor + (ny + 1) * nx * (k-1) + nx * (j - 1) + i) = ...
                    v(helper_y)-v(helper_y + nx);
                helper_y = helper_y + 1;
            end
        end
    end
end
add_factor = add_factor + (ny+1)*nx*nz;
for k=1:(nz+1)
    for j=1:ny
        for i=1:nx
            if (k == 1)
                res(add_factor + (ny+1)*nx*(k-1)+nx*(j-1)+i) = ...
                    -v(helper_z - 1 + (j - 1) * nx + i);
            elseif (k == (nz+1))
                res(add_factor + nx * ny * nz + (j-1)*nx + i) = ...
                    v(helper_z);
                helper_z = helper_z + 1;
            else
                res(add_factor + ny * nx * (k-1) + nx * (j-1) + i) = ...
                    v(helper_z)-v(helper_z + nx*ny);
                helper_z = helper_z + 1;
            end
        end
    end
end

else % trans = 'N' || trans = 'n'
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% D * v %%%%%%%%%%%%%%%%%%%%%%%%%%%%
for k=1:nz
    for j=1:ny
        for i=1:(nx+1)
            if (i ~= 1)
                res((i-1) + (j-1)*nx + (k-1)*nx*(ny)) = ...
                    res((i-1) + (j-1)*nx + (k-1)*nx*(ny)) ...
                    - v((i-1) + (j-1)*(nx+1) + (k-1)*(nx+1)*(ny));
            end
            if (i ~= (nx+1))
                res(i + (j-1)*nx + (k-1)*nx*(ny)) = ...
                    res(i + (j-1)*nx + (k-1)*nx*(ny)) ...
                    + v(i + 1 + (j-1)*(nx+1) + (k-1)*(nx+1)*(ny));
            end
        end
    end
end
add_factor = (nx+1)*ny*nz;
for k=1:nz
    for j=1:(ny+1)
        for i=1:nx
            if (j ~= 1)
                res(i + (j-2)*nx + (k-1)*nx*ny) = ...
                    res(i + (j-2)*nx + (k-1)*nx*ny) ...
                    - v(add_factor + i + (j-2)*nx + (k-1)*nx*(ny+1));
            end
            if (j ~= (ny+1))
                res(i + (j-1)*nx + (k-1)*nx*ny) = ...
                    res(i + (j-1)*nx + (k-1)*nx*ny) ...
                    + v(add_factor + i + j*nx + (k-1)*nx*(ny+1));
            end
        end
    end
end
add_factor = add_factor + nx*(ny+1)*nz;
for k=1:(nz+1)
    for j=1:ny
        for i=1:nx
            if (k ~= 1)
                res(i + (j-1)*nx + (k-2)*nx*ny) = ...
                    res(i + (j-1)*nx + (k-2)*nx*ny) ...
                    - v(add_factor + i + (j-1)*nx + (k-2)*nx*ny);
            end
            if (k ~= (nz+1))
                res(i + (j-1)*nx + (k-1)*nx*ny) = ...
                    res(i + (j-1)*nx + (k-1)*nx*ny) ...
                    + v(add_factor + i + (j-1)*nx + (k)*nx*ny);
            end
        end
    end
end
end



