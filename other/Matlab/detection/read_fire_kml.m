function v=read_fire_kml(filename)
% v=read_fire_kml(filename)
% Read fire detection kml file from FireMapper
%
% Input:  filename    first gunzip the kmz file to get kml
%        
% Output:
%         v.lon       longitude
%         v.lat       latitude
%         v.tim       detection time (days, use datestr to convert to a string)

fid=fopen(filename);
if fid < 0
    error(['could not find file ',filename])
end
k=0;
s=10000;
v.lat=zeros(s,1);
v.lon=zeros(s,1);
v.tim=zeros(s,1);
while 1
    fline = fgetl(fid);
    if ~ischar(fline),
        fclose(fid)
        break
    end % end of file
    f=strfind(fline,'Fire Detection Centroid ');
    if ~isempty(f)
        dline = fgetl(fid);
        if ~ischar(dline),
            error('missing next line'),
        end % end of file
         k=k+1;
        flat=parse(dline,'<b>Latitude: </b>','<br/>');
        flon=parse(dline,'<b>Longitude: </b>','<br/>');
        fdate=parse(dline,'<b>Detection Date: </b>','<br/>');
        ftime=parse(dline,'<b>Detection Time: </b>','<br/>');
        v.lat(k)=str2num(flat);
        v.lon(k)=str2num(flon);
        v.tim(k)=datenum([fdate,' ',ftime]);
    end
end
v.lat=v.lat(1:k);
v.lon=v.lon(1:k);
v.tim=v.tim(1:k);
end

