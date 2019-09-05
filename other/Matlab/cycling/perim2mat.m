% make .mat files from a kml for use in data assimilation
% requires mapping toolbox and the kml2struct.m function

kmlFile = 'doc.kml';
%a = kml2struct(kmlFile);
%load('perim_struct.mat');

%look at the kml file
p_count = 0;
%hold on

%%%%%%% start 13 for special point %%%%%%%%
% number of points in grid to use
n = 100;
for i = 13:length(a)
    i, a(i)
    if strcmp(a(i).Geometry,'Polygon')
        p_count = p_count + 1;        
        %i,a(i)
        
        %set decimate to an  postive integer to use just a subset of points
        %  in perimeter
        decimate = 1;
        lats = a(i).Lat(1:decimate:end);
        lons = a(i).Lon(1:decimate:end);

        %create regularly spaced data
        dx = (a(i).BoundingBox(2,1)-a(i).BoundingBox(1,1))/n;
        dy = (a(i).BoundingBox(1,2)-a(i).BoundingBox(2,2))/n;
        
        xa = linspace(a(i).BoundingBox(1,1),a(i).BoundingBox(2,1),n);
        ya = linspace(a(i).BoundingBox(2,2),a(i).BoundingBox(1,2),n);
        
        %find data inside of perimeter
        [x,y] = meshgrid(xa,ya);
        x = x(:);
        y = y(:);
        [in,on] = inpolygon(x,y,lons,lats);
        fires = logical(in+on);
        data = reshape(fires,n,n);
        %make all high confidence fires
        data = uint8(9.0*data);
        a(i).data = data;
        geotransform = [ a(i).BoundingBox(1,1) dx 0  a(i).BoundingBox(2,2) 0 dy];
        a(i).geotransform = geotransform;
        %save the file for use in data assimilation
        %save(a(i).TIF_name,'data','geotransform');
        %plot results
        plot_on = 1;
        if plot_on
            figure
            hold on
            plot(lons,lats);
            scatter(x,y);
            scatter(x(fires),y(fires),'*','r');
            title(a(i).Name);
            plot(lons,lats)
            hold off
            %figure,mesh(data)
        end %if plot_on
        
    end
end %for

%save perim_struct.mat a
fprintf('There were %i perimeters found in the data set\n',p_count)


