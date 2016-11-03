function t=datenum2time(d,red)
    % convert from datenum to sec since simulation start
    t=(24*60*60)*(d - red.start_datenum);
end
