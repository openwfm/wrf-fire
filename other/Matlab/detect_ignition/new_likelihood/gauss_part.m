function [g_part ] = gauss_part( fire_x, fire_y, pixel_x, pixel_y, radius )
% compute gaussian part of the formula
% inputs :
%   fire_x, fire_y : [x y] location of pixel fire pixels
%   pixel_x, pixel_y : [x y] location of where satellite is really looking
%   radius : std. deviation of the Gaussian, made large for first runs
%   
% outputs:
%   g_part: sum of the gaussians

%r = 200.0;  %200 m pixel ->>> 300 m radius at 3 sigma uncertainty
weight = 1;

dist2 = (fire_x-pixel_x)^2+(fire_y-pixel_y)^2;
g_part = weight*exp(-dist2/(3*radius)^2);


end
