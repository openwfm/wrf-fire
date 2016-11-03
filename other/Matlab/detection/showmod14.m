function showmod14(v,varargin)
% showmod14(v,alpha,string)
% display MODIS14 data
% from loading file converted by geotiff2mat.py
% input 
%   v.data     MODIS 14 data
%   v.geo      geotransform from geotiff
%   v.file    optional title string
%   v.axis    min, max longgitude. latitude

% newplot

if length(varargin)>0,
    alpha=varargin{1};
else
    alpha=1;
end
pixels_fire=[sum(v.data(:)==7),sum(v.data(:)==8),sum(v.data(:)==9)];
if length(varargin)>1,
    t=varargin{2};
else
    t = sprintf('%s %s fire pixels %i %i %i',v.file,...
    datestr(v.time,'yyyy-mm-dd HH:MM:SS'),pixels_fire);
end
cmap=cmapmod14;
alphadata=zeros(size(v.data));
alphamap=any(cmap,2)*alpha;
% alphamap=ones(size(alphamap));
d=v.data;
d(isnan(d))=0;
a=alphamap(d+1);
image('Xdata',[v.lon(1),v.lon(end)],'Ydata',[v.lat(1),v.lat(end)],...
    'Cdata',v.data,'Alphadata',a);
a=gca;set(a,'Ydir','normal')
colormap(cmap);
xlabel('Longitude (deg)')
ylabel('Latitude (deg)')
title(t,'Interpreter','none')
if isfield(v,'axis'),
    axis(v.axis);
end
daspect([1,cos(mean(v.lat(:))*pi/180),1]);

% grid on
% show fire pixels
% hold on, [i,j,c]=find(data > 6); plot(lat(j),lon(i),'+r'), hold off
end

