function print_time_bounds(red,str,time1,time2)
    fprintf('%-10s datenum from %20.13g to %20.13g\n from %s to %s\n',...
            str,time1,time2,stime(time1,red),stime(time2,red))
end

