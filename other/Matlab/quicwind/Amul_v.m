function res = Amul_v(X,A,v)
% multiplies the flux matrix (or its transpose) and a vector
% the operation is res = A * v, where A is a diagonal matrix
% set:
%    X   mesh
%    A   matrix to multiply, must be diagonal
%    v   vector to multiply

% Error checking
check_mesh(X);

% Initialize result
res = zeros(size(v));

% Mesh sizes and vars
nx = size(X{1},1)-1;
ny = size(X{1},2)-1;
nz = size(X{1},3)-1;
x = X{1}; y = X{2}; z = X{3};

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% A * v %%%%%%%%%%%%%%%%%%%%%%%%%%%%
for k=1:nz
    for j=1:ny
        for i=1:(nx+1)
            res(i + (j-1)*(nx+1) + (k-1)*(nx+1)*ny) = ...
                A(i + (j-1)*(nx+1) + (k-1)*(nx+1)*ny,i + (j-1)*(nx+1) + (k-1)*(nx+1)*ny) * v(i + (j-1)*(nx+1) + (k-1)*(nx+1)*ny);
        end
    end
end
add_factor = (nx+1)*ny*nz;
for k=1:nz
    for j=1:(ny+1)
        for i=1:nx
            res(add_factor + i + (j-1)*nx + (k-1)*nx*(ny+1)) = ...
                A(add_factor + i + (j-1)*nx + (k-1)*nx*(ny+1),add_factor + i + (j-1)*nx + (k-1)*nx*(ny+1)) * v(add_factor + i + (j-1)*nx + (k-1)*nx*(ny+1));
        end
    end
end
add_factor = add_factor + nx*(ny+1)*nz;
for k=1:(nz+1)
    for j=1:ny
        for i=1:nx
            res(add_factor + i + (j-1)*nx + (k-1)*nx*ny) = ...
                A(add_factor + i + (j-1)*nx + (k-1)*nx*ny,add_factor + i + (j-1)*nx + (k-1)*nx*ny) * v(add_factor + i + (j-1)*nx + (k-1)*nx*ny);
        end
    end
end