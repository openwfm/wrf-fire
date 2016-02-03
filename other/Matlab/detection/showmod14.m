function showmod14(file)
% display MODIS14 data
% from loading file converted by geotiff2mat.py
% input 
%   data     MODIS 14 data
%   geo      geotransform from geotiff
%   optional title string

v=readmod14(file);
image(v.lon,v.lat,v.data);
colormap(cmapmod14);
xlabel('Longitude (deg)')
ylabel('Latitude (deg)')
if ~exist('s','var'), s='MOD14'; end
t = sprintf('%s fire pixels %i %i %i',file,v.pixels.fire);
title(t,'Interpreter','none')
grid on
% show fire pixels
% hold on, [i,j,c]=find(data > 6); plot(lat(j),lon(i),'+r'), hold off
end

