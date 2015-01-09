function showmod14(data,geo,s)
% display MODIS14 data
% from loading file converted by geotiff2mat.py
% input 
%   data     MODIS 14 data
%   geo      geotransform from geotiff
%   optional title string
%
% black = notprocessed
% blue  = water
% white = cloud
% green = land
% red   = fire (saturation is confidence level)


cmap= [ ...
    0   0   0    %0 not processed (missing input data), black
    0   0   0    %1 not used, black
    0   0   0    %2 not processed (other reason)
    0   0   0.2    %3 water, dark blue
    0.1 0.2 0.2    %4 cloud, purple gray
    0   0.2   0    %5 non-fire clear land, green
    0   0   0    %6 unknown
    0.6 0.4 0.4    %7 low-confidence fire
    0.8 0.5  0.5    %8 nominal-confidence fire
    1.0 0.6  0.6    %9 high-confidence fire
];
% cmap= zeros(10,3);
% cmap(8:10,:)=1;
if any(data(:)<0 | data(:)>9), 
    warning('Value out of range 0 to 9 for MODIS14 data')
end

[rows,cols]=size(data);
fprintf('rows                  %i\n',rows)
fprintf('cols                  %i\n',cols)
fprintf('top left X            %19.15f\n',geo(1))
fprintf('W-E pixel resolution  %19.15f\n',geo(2))
fprintf('rotation, 0=North up  %19.15f\n',geo(3))
fprintf('top left Y            %19.15f\n',geo(4))
fprintf('rotation, 0=North up  %19.15f\n',geo(5))
fprintf('N-S pixel resolution  %19.15f\n',geo(6))
if geo(3)~=0 | geo(5)~=0,
    error('rotation not supported')
end
for i=1:10,
    count(i+1)=sum(data(:)==i);
end
unknown= count(1)+count(2)+count(3)+count(7);
water  = count(4);
cloud  = count(5);
land   = count(6);
fire   = count(8:10);
fprintf('unprocessed/unknown   %i\n',unknown)
fprintf('water                 %i\n',water)
fprintf('land                  %i\n',land)
fprintf('cloud                 %i\n',cloud)
fprintf('low-confidence fire   %i\n',fire(1))
fprintf('nominal-confid fire   %i\n',fire(2))
fprintf('high-confidence fire  %i\n',fire(3))

lon = geo(1)+[0:rows-1]*geo(2);
lat = geo(4)+[cols-1:-1:0]*geo(6);
image(lon,lat,data);
colormap(cmap);
xlabel('Longitude (deg)')
ylabel('Latitude (deg)')
if ~exist('s','var'), s='MOD14'; end
t = sprintf('%s fire pixels %i %i %i',s,fire);
title(t,'Interpreter','none')
grid on
% show fire pixels
% hold on, [i,j,c]=find(data > 6); plot(lat(j),lon(i),'+r'), hold off
end

