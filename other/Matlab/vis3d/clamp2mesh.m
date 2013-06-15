% function clamp2mesh
% compute the nearest point in the fire mesh to the given coordinates
% use to determine ignition points that lie on the mesh
d=input('enter domain number: ');
f=sprintf('wrfinput_d%02i',d);
fprintf('reading file %s\n',f);
lon=ncread(f,'FXLONG');
lat=ncread(f,'FXLAT');
min_x=min(lon(:));
max_x=max(lon(:));
min_y=min(lat(:));
max_y=max(lat(:));
[m,n]=size(lon);
[m1,n1]=size(lat);
if(m1 ~= m | n1 ~= n),
    error('inconsistent size of FXLONG and FXLAT')
end
fprintf('loaded mesh size %i by %i coordinates %g to %g by %g to %g\n',m,n,min_x,max_x,min_y,max_y);
if(max_x == 0 | max_y == 0)
    error('Fire mesh coordinates not set. Please use wrfinput file from a current real.exe or ideal.exe')
end
longlat=input('Enter 1=real run or 0=ideal: ');
if longlat==0, % ideal
    fprintf('ideal: ')
    unit_fxlon = 1;
    unit_fxlat = 1;
else
    fprintf('real: ')
    lon_ctr=mean(lon(:));
    lat_ctr=mean(lat(:));
    fprintf('the center of the domain is at coordinates %g %g\n',lon_ctr,lat_ctr)
    unit_fxlat=6370*2*pi/360;   % one degree latitude in m
    unit_fxlon=cos(lat_ctr*2*pi/360)*unit_fxlat; % one degree longitude in m
end
fprintf('coordinate units are %g %g m\n',unit_fxlat,unit_fxlon)
    
while 1
    x=input('enter the 1st coordinate of ignition point (x or longitude), or enter to exit: ');
    if isempty(x),
        return
    end
    y=input('enter the 2nd coordinate of ignition point (y or latitude), or enter to exit: ');
    if isempty(y),
        return
    end
    fprintf('entered coordinates %g %g\n',x,y)
    if (x<min_x | x>max_x | y<min_y | y>max_y),
        error('the point is outside of the domain')
    end
    % find the nearest point (lon(i,j),lat(i,j)) to (x,y)
    d = sqrt((unit_fxlon*(lon-x)).^2 + (unit_fxlat*(lat-y)).^2);
    [p,q,minvalue]=find(d==min(d(:)));
    for ii=1:length(p)
        i=p(ii);
        j=q(ii);
        fprintf('nearest mesh point %i %i at coordinates %12.10g %12.10g distance %g\n',...
            i,j,lon(i,j),lat(i,j),d(i,j))
    end
    
end
% end

