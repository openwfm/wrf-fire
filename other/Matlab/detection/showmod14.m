function showmod14(v)
% display MODIS14 data
% from loading file converted by geotiff2mat.py
% input 
%   v.data     MODIS 14 data
%   v.geo      geotransform from geotiff
%   v.file    optional title string
%   v.axis    min, max longgitude. latitude

% newplot
cmap=cmapmod14;
alphadata=zeros(size(v.data));
alphamap=any(cmap,2);
a=alphamap(v.data+1);
image('Xdata',[v.lon(1),v.lon(end)],'Ydata',[v.lat(1),v.lat(end)],...
    'Cdata',v.data,'Alphadata',a);
a=gca;set(a,'Ydir','normal')
pixels_fire=[sum(v.data(:)==7),sum(v.data(:)==8),sum(v.data(:)==9)];
colormap(cmap);
xlabel('Longitude (deg)')
ylabel('Latitude (deg)')
t = sprintf('%s %s fire pixels %i %i %i',v.file,...
    datestr(v.time,'yyyy-mm-dd HH:MM:SS'),pixels_fire);
title(t,'Interpreter','none')
if isfield(v,'axis'),
    axis(v.axis);
end
% grid on
% show fire pixels
% hold on, [i,j,c]=find(data > 6); plot(lat(j),lon(i),'+r'), hold off
end

