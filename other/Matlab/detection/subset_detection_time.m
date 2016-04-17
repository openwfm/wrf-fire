function time_bounds=subset_detection_bounds(red,p)

min_det_time=p.time(1);
max_det_time=p.time(end);
      
% choose time bounds
print_time_bounds('Simulation',red.min_tign,red.max_tign)
print_time_bounds('Detections',min_det_time,max_det_time)
b1=max(red.min_tign,min_det_time);
b2=min(red.max_tign,max_det_time);
ba=0.5*(b1+b2);
bd=b2-b1;
default_time_bounds{1}=[b1,b2];
default_time_bounds{2}=[b1,b1+0.3*bd];
default_time_bounds{3}=[b1,b1+0.5*bd];
default_time_bounds{4}=[ba-0.2*bd,ba+0.2*bd];
for i=1:length(default_time_bounds)
    str=sprintf('bounds %i',i);
    print_time_bounds(str,default_time_bounds{i}(1),default_time_bounds{i}(2)) 
end
time_bounds=input_num('bounds [min_time max_time] from start time, or number of bounds above',3);
if length(time_bounds)==1, 
    time_bounds=default_time_bounds{time_bounds};
else
    time_bounds=time_bounds+red.min_tign;
end
print_time_bounds('Using bounds',time_bounds(1),time_bounds(2))

  
    function print_time_bounds(str,time1,time2)
        fprintf('%-10s from %s to %s\n',str,stime(time1,red),stime(time2,red))
    end

end