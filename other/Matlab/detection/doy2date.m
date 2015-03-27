function d=doy2date(s)
% convert geotime string yyjjjhhmmss to date string
dayspermonth = [0 31 28 31 30 31 30 31 31 30 31 30 31];
year=str2num(s(1:2));
dayofyear=str2num(s(3:5));
hours=str2num(s(6:7));
minutes=str2num(s(8:9));
seconds=str2num(s(10:11));
if mod(year,4)==0 & mod(year,100) ~=0,
    dayspermonth(2)=29; % leap year
end
c = cumsum(dayspermonth);
idx=find(dayofyear<=c);
if isempty(idx),
    error('bad day of the year')
end
month = idx(1)-1;
dayofmonth = dayofyear - c(month);
d = datestr([year+2000,month,dayofmonth,hours,minutes,seconds]);
end

