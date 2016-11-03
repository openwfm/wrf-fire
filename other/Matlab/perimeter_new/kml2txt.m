function [] = kml2text(fileName,outName,thinning_factor)
%kml2text reads a Google Earth kml file (lat,lon,z) from a fire perimeter 
%file and saves a text file for the marching algorithm used in
%main_function

%% open the data file and find the beginning of the data
%fileName='UT-SLD-HU2S Patch Springs 8-12-2013 2123.kml';
display(fileName)

fid=fopen(fileName);
if fid < 0
    error('could not find input file')
end
done=0;
i=0;
%% Read in data and look for the <coordinate> string
while done == 0
    i=i+1;
    readlines{i,:}=fgetl(fid);
    f{i}=findstr(readlines{i,:},'<coordinates>');
    if isempty(f{i}==1) % set to zero if no <coordinates> tag
        coord_tag_line(i)=0;  
    else  %set coord_tag_line to one for lines with <coordinates>
        coord_tag_line(i)=f{i};
    end
    
    if readlines{i,:} == -1 % when passes through the end of the file fgetl returns -1 if that happens set done to 1 and finish teh loop
        done = 1;
    end
end

%% find line numbers of the file where <coordinate> string and data are
coord_tag_lines=find(coord_tag_line==4);
%% find lines with <coordinate> string only, the lines with actual coordinates will be one below heance +1
coord_lines=find(coord_tag_line==6)+1;

ar=1;

%% read coordinate data lines w/o tags
for ii=1:size(coord_lines)
        rawdata=readlines{coord_lines(ii)};
        alldata{ar} = rawdata(6:(size(rawdata,2)-16));
        ar = ar + 1;
end


%% get the data into neat vectors

    % turn alldata into regular vector so it is easier to work with
    data = cell2mat(alldata);
    % now find all commas
    fComma = strfind(data, ',');
    % find all spaces
    fSpace = strfind(data,' ');
    % find all tabs
    fTab = strfind(data,'	');
    a=1;
    fC = 1;
    
    % have to do first point seperately b/c line may not begin with a space

    lon(a) = str2num(data(1:fComma(fC)-1));
     lat(a) = str2num(data(fComma(fC)+1:fComma(fC+1)-1));
     z(a) = str2num(data(fComma(fC+1)+1:fSpace(1)-1));
     a=a+1;
     fS=1;

    % go thru all the points in the line
    for fC = 3: 2: length(fComma)
        
            lon(a) = str2num(data(fSpace(fS)+1:fComma(fC)-1));
            lat(a) = str2num(data(fComma(fC)+1:fComma(fC+1)-1));
        if fS  < length(fSpace)
            z(a) = str2num(data(fComma(fC+1)+1:fSpace(fS+1)-1 ));
        else
            % have to handle last point seperatly b/c line may not end with
            % a space
            z(a) = str2num(data(fComma(fC+1)+1:end ));
        end
        a=a+1;
        fS=fS+1;
    end
    
    %%Process points with the <coordinate> tag
         br=1;
         fC=1;
         b=1;
 for ii=1:size(coord_tag_lines)
         rawdata=readlines{coord_tag_lines(ii)};
         alldata2{br} = rawdata(17:(size(rawdata,2)-14));
         br = br + 1;
 end

%  data2=cell2mat(alldata2);
%  
%     fComma2 = strfind(data2, ',');
%     % find all spaces
%     fSpace2 = strfind(data2,' ');
%     % find all tabs
%     fTab2 = strfind(data2,'	');
%    
%     lon_point(b) = str2num(data2(1:fComma2(fC)-1));
%     lat_point(b) = str2num(data2(fComma2(fC)+1:fComma2(fC+1)-1));
%     z_point(b) = str2num(data2(fComma2(fC+1)+1:end));
    
       
    
fclose(fid);
[a,b]=size(lat);
lat=reshape(lat,max(a,b),min(a,b));
lon=reshape(lon,max(a,b),min(a,b));
z=reshape(z,max(a,b),min(a,b));

outName

fileid=fopen(outName,'w');
for i=1:thinning_factor:size(lat,1);
    fprintf(fileid,'%21.16f %21.16f %1.0f\n',lon(i,1),lat(i,1),z(i,1));
end
display ('Output file written successfuly')

