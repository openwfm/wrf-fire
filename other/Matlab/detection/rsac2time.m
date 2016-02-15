function [t,varargout]=rsac2time(name)
% convert rsac MODIS/VIIRS file names to time value
str=regexp(name,'\.[0-9]+\.','match');
str=str{1};
str=['20',str(2:end-1)];  %yyyydddHHMMSSS
t=julian2time(str);
if nargout>1,
    varargout{1}=str;
end
end

