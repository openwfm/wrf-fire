
%local machine
%prefix = '/home/jhaley/JPSSdata/';
%colibri
prefix = '/bigdisk/james.haley/wrfcycling/wrf-fire/wrfv2_fire/test/TIFs/';


%Longitude = hdfread('/home/jhaley/JPSSdata/MOD03.A2013222.0545.006.2013222112442.hdf', 'MODIS_Swath_Type_GEO', 'Fields', 'Longitude');
%Latitude = hdfread('/home/jhaley/JPSSdata/MOD03.A2013222.0545.006.2013222112442.hdf', 'MODIS_Swath_Type_GEO', 'Fields', 'Latitude');
%fire_mask = hdfread('/home/jhaley/JPSSdata/MOD14.A2013222.0545.006.2015263221706.hdf', '/fire mask', 'Index', {[1  1],[1  1],[2030  1354]});

dhdf=dir([prefix,'*.hdf']);
dh5 = dir([prefix,'*.h5']);
dnc = dir([prefix,'*.nc']);
d=[{dhdf.name},{dh5.name},{dnc.name}];
%d={d.name};

if(isempty(d)), error(['No files found for ',prefix]),end

% order the files in time
nfiles=length(d);
t=zeros(1,nfiles);
for i=1:nfiles
    f{i}=[prefix,d{i}];
    t(i)=rsac2time(d{i});
end

[t,i]=sort(t);
p.file={d{i}};
p.time=t;

fires = [0 0]
for k = 1:nfiles
    if mod(k,2) == 1
        file = p.file{k};
        file2 = p.file{k+1};
        v = readl2data(prefix,file,file2);
        
        dets = v.data >= 7;
        v
        if sum(v.pixels.fire) > 0
            fires = [fires; v.lon(dets) v.lat(dets)];
            %fires = [fires; v.lons(dets) v.lats(dets)];
        end
        
    end
end

fires = fires(2:end,:);
figure,scatter(fires(:,1),fires(:,2))

