function [ ] = quick_mesh(varargin)
%msg = quick_mesh(matrix)
%   function meshes only a subset of a matrix, for quick handling

if nargin == 1
    matrix = varargin{1};
    figure;
end


if nargin == 2
    matrix = varargin{1};
    fig_num = varargin{2};
    figure(fig_num);
end

if nargin > 2
    error('Too many arguments')
end

mesh(matrix(1:10:end,1:10:end));
%msg = 'mesh(matrix(1:10:end,1:10:end))'

end

