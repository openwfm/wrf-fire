function [ perim_struct ] = shape2struct( prefix )
%[ perim_struct ] = shape2struct( prefix )
% inputs:
%   prefix - string, path to directory with shapefiles
% output:
%   perim_struct - structure with perimeter information

% to do:
% put names in sthe struct file as in UT-SLD-HU2S Patch Springs 8-20-2013
% 2150

d=dir([prefix,'*.shp'])
d={d.name};
if(isempty(d)), error(['No files found for ',prefix]),end

% order the files in time
nfiles=length(d);
t=zeros(1,nfiles);
%date_format = 'yyyymmdd_HHMM';
date_format = 'mm-dd-yyyy_HHMM';
%perim times are local, need to convert to UTC
zone_shift = -8;


for i=1:nfiles
    f{i}=[prefix,d{i}];
    date = d{i}(end-18:end-4);
    %t(i) = datenum(date,date_format)+zone_shift/24;
    t(i) = datenum(date,date_format);
end
[t,i]=sort(t);
p.file={d{i}};
p.time=t;

%combine into struct
for i = 1:length(p.file)
    perim_struct(i) = shaperead(f{i})
    %perim_struct(i).Lon = perim_struct(i).X
    %perim_struct(i).Lat = perim_struct(i).Y 
end

for i = 1:length(p.file)
    %perim_struct(i) = shaperead(f{i})
    perim_struct(i).Lon = perim_struct(i).X;
    perim_struct(i).Lat = perim_struct(i).Y;
    perim_struct(i).Name = replace(p.file{i},'_',' ');
    perim_struct(i).Name = perim_struct(i).Name(1:end-4);
end

% perim_struct.Lon = perim_struct.X
% perim_struct.Lat = perim_struct.Y
end

