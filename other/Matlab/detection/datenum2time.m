function t=datenum2time(d,red)
    % convert from datenum to sec since simulation start
    t=red.max_tign_g + (24*60*60)*(d - red.time);
end
