function t=rsac2time(name)
% convert rsac MODIS/VIIRS file names to time value
str=regexp(name,'\.[0-9]+\.','match');
str=str{1};
str=str(2:end-1);  %yydddHHMMSSS
t=julian2time(['20',str]);
end

