function [t,varargout]=rsac2time(name)
% convert rsac MODIS/VIIRS file names to time value
str=regexp(name,'\.[0-9]+\.','match');
if isempty(str),
    error('NASA time string not found')
end
% detect level data, file name is not *.mat
if name(end) ~= 't'
    %name
    s1 = regexp(name,'\.[A-0]+\.','match');
    s1 = s1{1};
    s1 = s1(3:end-1);
    s2 = regexp(name,'\.[0-9]+\.','match');
    s2 = s2{1};
    s2 = [s2(2:end-1),'00'];
    str = [s1,s2];
%read *.tif.mat file
else
    str=regexp(name,'\.[0-9]+\.','match');
    str=str{1};
    str=['20',str(2:end-1)];  %yyyydddHHMMSSS
end

t=julian2time(str);
if nargout>1,
    varargout{1}=str;
end
end

