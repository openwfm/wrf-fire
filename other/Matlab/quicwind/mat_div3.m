function Dmat=mat_div3(X,h)
% Dmat=mat_div3(X)
% Compute divergence matrix for a staggered mesh
% arguments:
%    X      staggered mesh (midpoints of sides)
%    h(1:3) mesh step
% output: 
%    Dmat   divergence matrix (sparse) on mesh
%
if ~exist('h','var')
    h=[1,1,1];
end
% Mesh computations
x = X{1}; y = X{2}; z = X{3};
% Q: this assumes that we are working on a cube based on the X direction?
% A: no, the size of all X{i} should be the same, error check
[nx1, ny1, nz1] = size(x);
nx = nx1 - 1; ny = ny1 - 1; nz = nz1 - 1;
% TODO: error check to make sure we are only working in 3 dimensions?
% A: yes
n = nx * ny * nz1 + nx * ny1 * nz + nx1 * ny * nz; 

% Compute number of nonzeros
%   Each boundary cell accounts for 1 nonzero, each non-boundary cell adds
%   2 (in each dimension)
% TODO: check this math
nnz = (nx1 - 2) * ny * nz + ny * nz + ...
    (ny1 - 2) * nx * nz + nx * nz + ...
    (nz1 - 2) * nx * ny + nx * ny;
nnz = 2 * nnz;

% For sparse matrix
I = zeros(nnz,1);
J = zeros(nnz,1);
D = zeros(nnz,1); % Value of Dmat at index (I,J)

D_col = 1;
D_entry = 1; % Current entry of Dmat
for ux=1:numel(X)
    
    % Size of the grid for the current direction
    grid_size = [nx,ny,nz];
    grid_size(ux) = grid_size(ux) + 1;
    X_len = grid_size(1); Y_len = grid_size(2); Z_len = grid_size(3);
    
    % Main loop, iterates through all columns of Dmat
    for kx=1:Z_len
        for jx=1:Y_len
            for ix=1:X_len
                
                % Edge cases and relevant indices
                if (ux == 1)
                    low_row_ind = ix - 1 + (jx - 1) * nx + (kx - 1) * ny * nx;
                    row_ind = ix + (jx - 1) * nx + (kx - 1) * ny * nx;
                    is_lower_edge = ix == 1;
                    is_upper_edge = ix == X_len;
                elseif (ux == 2)
                    low_row_ind = ix + (jx - 2) * nx + (kx - 1) * ny * nx;
                    row_ind = ix + (jx - 1) * nx + (kx - 1) * ny * nx;
                    is_lower_edge = jx == 1;
                    is_upper_edge = jx == Y_len;
                else
                    low_row_ind = ix + (jx - 1) * nx + (kx - 2) * ny * nx;
                    row_ind = ix + (jx - 1) * nx + (kx - 1) * ny * nx;
                    is_lower_edge = kx == 1;
                    is_upper_edge = kx == Z_len;
                end

                if is_lower_edge
                    % Special case for top boundary
                    D(D_entry) = -1 / h(ux);
                    I(D_entry) = row_ind;
                    J(D_entry) = D_col;
                    D_entry = D_entry + 1;
                elseif is_upper_edge
                    % Special case for lower boundary
                    D(D_entry) = 1 / h(ux);
                    I(D_entry) = low_row_ind;
                    J(D_entry) = D_col;
                    D_entry = D_entry + 1;
                else
                    % Other elements
                    D(D_entry) = 1 / h(ux);
                    I(D_entry) = low_row_ind;
                    J(D_entry) = D_col;
                    D_entry = D_entry + 1;
                    D(D_entry) = -1 / h(ux);
                    I(D_entry) = row_ind;
                    J(D_entry) = D_col;
                    D_entry = D_entry + 1;
                end
            D_col = D_col + 1;
            end
        end
    end
end
Dmat = sparse(I, J, D, nx * nz *ny, n);

