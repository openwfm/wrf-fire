function plot_loglike(fig,f,s,red)
% plot_loglike(fig,f,s,red)
%
% display array in a way suitable for log likelihood
% overay with fireline at end of observations = time_bounds(2) 
%   fig     figure number
%   f       array to show
%   s       title
%   red     the reduced structure with everyhing
figure(fig)
hold off, clf
% show field f
pcolor(red.fxlong,red.fxlat,f)
shading interp
colormap default
colorbar
hold on
% add fireline
contour3(red.fxlong,red.fxlat,red.tign-red.time_bounds(2),[0 0],'k')
hold off
title(s)
end
