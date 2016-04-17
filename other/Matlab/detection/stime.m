function str=stime(t,red)
% str=stime(t,red)
% convert time as datenum to string and day from base_time
% in:
%   t    time as datenum
%   red  struct with field base_time
% out:
%   str  char array

    timefmt='dd-mmm-yyyy HH:MM:SS';
    if ~isscalar(t) | ~isnumeric(t),
        error('t must be a number')
    end
    str=sprintf('%s day %g',datestr(t,timefmt),t-red.base_time);
end
