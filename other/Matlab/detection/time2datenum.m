function d=time2datenum(t,red)
% convert time in seconds since sim start to datenum
% t     time in seconds since sim start
% red   structure with fields:
%       red.max_tign_g    a reference time as  seconds since sim start
%       red.time          the same time as datenum
% d     time as datenum
d=(t - red.max_tign_g)/(24*60*60) + red.time;
end