function [g_part ] = gauss_part( fire_x, fire_y, pixel_x, pixel_y, sig )
% compute gaussian part of the formula
% inputs :
%   fire_x, fire_y : [x y] location of pixel fire pixels (reported
%   locations...)
%   pixel_x, pixel_y : [x y] location of where satellite is really looking
%   sig  :standard deviation
%   
% outputs:
%   g_part: sum of the gaussians

%r = 200.0;  %200 m pixel ->>> 300 m radius at 3 sigma uncertainty
%sig = 1;

dist2 = (fire_x-pixel_x)^2+(fire_y-pixel_y)^2;
g_part = exp(-dist2/(2*sig^2));


end
