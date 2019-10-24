function [ ] = quick_mesh(varargin)
%msg = quick_mesh(matrix)
%   function meshes only a subset of a matrix, for quick handling

if nargin == 1
    matrix = varargin{1};
    figure;
    mesh(matrix(1:5:end,1:5:end));
    return
end


if nargin == 2
    matrix = varargin{1};
    fig_num = varargin{2};
    figure(fig_num);
    mesh(matrix(1:10:end,1:10:end));
    return
end


if nargin == 3
    skip = 7;
    figure
    x =  varargin{1}; % x,y = meshgrid
    y =  varargin{2};
    z =  varargin{3}; % same size as x and y
    lons = x(1,1:skip:end);
    lats = y(1:skip:end,1);
    small_z = z(1:skip:end,1:skip:end);
    [m,n] = size(small_z);
    %c = lons.*lats;
    CO = zeros(m,n,3);
    CO(:,:,1) = zeros(m,n); % red
    CO(:,:,2) = small_z/max(small_z(:));
    %CO(:,:,2) = ones(m,1).*linspace(0.5,0.6,m); % green
    %CO(:,:,3) = ones(m,1).*linspace(0,1,m); % blue
    CO(:,:,3) = small_z/max(small_z(:))/2;
    c = small_z;
    %mesh(lons,lats,small_z,CO)
    mesh(lons,lats,small_z,'FaceAlpha',0.5,'EdgeColor','interp','FaceColor','interp','LineStyle',':')
    xlabel('Lon (degrees)')
    ylabel('Lat (degrees)')
%    figure
    hold on
    z_level = [1.7279e+05 1.7279e+05];
    contour3(lons,lats,small_z,z_level,'k')
    hold off
%     xlabel('Lon (degrees)')
%     ylabel('Lat (degrees)')
    
    
end


%msg = 'mesh(matrix(1:10:end,1:10:end))'

end

