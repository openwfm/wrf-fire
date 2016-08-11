function [ ] = write_kml( in_struct )
% Function writes a kml file based on input of in_struct, must contain logs
% Use one of the layers in the layers array structure
% example --> write_kml(layers(1)), layers(1) is the in_struct
%[m n] = size(in_struct.pts);
[m n] = size(in_struct.names);
%open file
fileID = fopen(in_struct.kml,'w');


%kml header
fprintf(fileID,'<?xml version="1.0" encoding="UTF-8"?> \n');
fprintf(fileID,'<kml xmlns="http://www.opengis.net/kml/2.2"> \n');
fprintf(fileID,'<Document> \n');

%create icons %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%get an integer from ignition time
% only works for ignition times which are multiples of 3600% 
% need to change this
int_time = (in_struct.time/3600 - 1)/2 +1;
pins =cellstr(['http://maps.google.com/mapfiles/kml/pushpin/blue-pushpin.png  ';
       'http://maps.google.com/mapfiles/kml/pushpin/grn-pushpin.png   ';
       'http://maps.google.com/mapfiles/kml/pushpin/ltblu-pushpin.png ';
       'http://maps.google.com/mapfiles/kml/pushpin/pink-pushpin.png  ';
       'http://maps.google.com/mapfiles/kml/pushpin/purple-pushpin.png';
       'http://maps.google.com/mapfiles/kml/pushpin/red-pushpin.png   ';
       'http://maps.google.com/mapfiles/kml/pushpin/wht-pushpin.png   ';
       'http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png   ';
       'http://maps.google.com/mapfiles/kml/pushpin/blue-pushpin.png  ';
       'http://maps.google.com/mapfiles/kml/pushpin/grn-pushpin.png   ';
       'http://maps.google.com/mapfiles/kml/pushpin/ltblu-pushpin.png ';
       'http://maps.google.com/mapfiles/kml/pushpin/pink-pushpin.png  ';
       'http://maps.google.com/mapfiles/kml/pushpin/purple-pushpin.png';
       'http://maps.google.com/mapfiles/kml/pushpin/red-pushpin.png   ';
       'http://maps.google.com/mapfiles/kml/pushpin/wht-pushpin.png   ';
       'http://maps.google.com/mapfiles/kml/pushpin/ylw-pushpin.png   ']);
fprintf(fileID,'   <Style id = "%s">\n',in_struct.run);
fprintf(fileID,'      <IconStyle>\n');
fprintf(fileID,'         <scale>1.0</scale>\n');
fprintf(fileID,'         <Icon><href>%s</href></Icon>\n',pins{int_time});
fprintf(fileID,'      </IconStyle>\n');
fprintf(fileID,'   </Style>\n');

%create folder
fprintf(fileID,'   <Folder> \n');
fprintf(fileID,'      <name>%s</name>\n',in_struct.run);
fprintf(fileID,'      <description>\n');
fprintf(fileID,'         Ignition times %d seconds after simulation start.\n',in_struct.time);
fprintf(fileID,'      </description>\n');
%loop to create placemarks
for i=1:m
    fprintf(fileID,'      <Placemark>\n');
    fprintf(fileID,'         <name>%s</name> \n',in_struct.names{i});
    fprintf(fileID,'         <styleUrl>#%s</styleUrl>\n',in_struct.run);
    fprintf(fileID,'         <description>\n');
    fprintf(fileID,'            j3 = %.4E \n',in_struct.logs(i));
    fprintf(fileID,'            lon = %.4f \n',in_struct.pts(i,1));
    fprintf(fileID,'            lat = %.4f \n',in_struct.pts(i,2));
    fprintf(fileID,'            t = %d seconds \n',in_struct.time);
    fprintf(fileID,'         </description> \n');
    fprintf(fileID,'         <Point> \n');
    fprintf(fileID,'            <extrude>1</extrude> \n');
    fprintf(fileID,'            <altitudeMode>absolute</altitudeMode>\n');
    %% set height of pushpin, needs to be changed so that it is not dependent
	% on this set of data
    fprintf(fileID,'            <coordinates>%4.3f,%4.3f,%5.3f</coordinates> \n',in_struct.pts(i,2),in_struct.pts(i,1),in_struct.logs(i)/10+9000);
    fprintf(fileID,'         </Point> \n');
    fprintf(fileID,'      </Placemark>\n');
end

%close folder
fprintf(fileID,'   </Folder> \n');

%close document
fprintf(fileID,'</Document>\n');
fprintf(fileID,'</kml>');


fclose(fileID);
end

