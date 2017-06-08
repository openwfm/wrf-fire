function t=add_datenum(t)
% t=add_datenum(t)
%
% add start and end times in datenum format

t.end_datenum = datenum(t.times(end,:)); % in days from some instant in the past
end_minutes=t.xtime(end); % from simulation start
t.start_datenum=t.end_datenum-end_minutes/(24*60);
t.tign_datenum = t.tign_g/82800+t.start_datenum;
