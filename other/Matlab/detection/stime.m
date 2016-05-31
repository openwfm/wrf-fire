function str=stime(t,red)
% str=stime(t,red)
% convert time given as datenum to string and day from start
% in:
%   t    time as datenum
%   red  struct with field base_time
% out:
%   str  char array

    timefmt='dd-mmm-yyyy HH:MM:SS';
    if ~isscalar(t) | ~isnumeric(t),
        error('t must be a number')
    end
    str=sprintf('%s %g days from start',datestr(t,timefmt),...
        t-red.start_datenum)
end
