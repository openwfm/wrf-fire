function v=readmod14(file)
% v=procmod14(file)
% read modis data and return v.lon v.lat v.data

v=load(file);
v.time=rsac2time(file);
if any(v.data(:)<0 | v.data(:)>9), 
    warning('Value out of range 0 to 9 for MODIS14 data')
end
[rows,cols]=size(v.data);
geo=v.geotransform;
v.lon = geo(1)+[0:rows-1]*geo(2);
v.lat = geo(4)+[cols-1:-1:0]*geo(6);

for i=1:10,
    count(i+1)=sum(v.data(:)==i);
end

% prints
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
v.pixels.unknown= count(1)+count(2)+count(3)+count(7);
v.pixels.water  = count(4);
v.pixels.cloud  = count(5);
v.pixels.land   = count(6);
v.pixels.fire   = count(8:10);
fprintf('unprocessed/unknown   %i\n',v.pixels.unknown)
fprintf('water                 %i\n',v.pixels.water)
fprintf('land                  %i\n',v.pixels.land)
fprintf('cloud                 %i\n',v.pixels.cloud)
fprintf('low-confidence fire   %i\n',v.pixels.fire(1))
fprintf('nominal-confid fire   %i\n',v.pixels.fire(2))
fprintf('high-confidence fire  %i\n',v.pixels.fire(3))
end