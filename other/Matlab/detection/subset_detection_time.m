function time_bounds=subset_detection_time(red,p)

% changes %%%%%%%%%%%%%%%%%%%%%%%%%%%
% lines 21, 27, 28 : accept defaults instead of sending to input_num

min_det_time=p.time(1);
max_det_time=p.time(end);
      
% choose time bounds
print_time_bounds(red,'Simulation',red.start_datenum,red.end_datenum)
print_time_bounds(red,'Detections',min_det_time,max_det_time)
b1=max(red.min_tign,min_det_time);
b2=min(red.max_tign,max_det_time);
ba=0.5*(b1+b2);
bd=b2-b1;
default_time_bounds{1}=[b1,b2];
default_time_bounds{2}=[b1,b1+0.3*bd];
default_time_bounds{3}=[b1,b1+0.5*bd];
default_time_bounds{4}=[ba-0.2*bd,ba+0.2*bd];
% default 5, based on namelist start time = 25200
default_time_bounds{5} = [735457.364874381,735459.979398148];
% to get ~19 detections from coarse grid runs
default_time_bounds{6} = [735457.4049189,735458.6921585];
for i=1:length(default_time_bounds)
    str=sprintf('bounds %i',i);
    print_time_bounds(red,str,default_time_bounds{i}(1),default_time_bounds{i}(2)) 
end
%time_bounds=input_num('bounds [min_time max_time] as datenum, or number of bounds above',3);
% set bounds 1
time_bounds=6;
if length(time_bounds)==1, 
    time_bounds=default_time_bounds{time_bounds};
end
print_time_bounds(red,'Using bounds',time_bounds(1),time_bounds(2))

%time_bounds(4)=input_num('perimeter time ',time_bounds(2));
time_bounds(4)=time_bounds(2)
%aa=input_num('Spinup period (h)',12);
aa=12;
%time_bounds(3)=input_num('restart time ',rounddatenum2hours(time_bounds(4)-aa/24));
time_bounds(3)= rounddatenum2hours(time_bounds(4)-aa/24);

print_time_bounds(red,'Spinup from restart to perimeter time',time_bounds(3),time_bounds(4))

end

function r=rounddatenum2hours(t)
    % input t time in datenum format (days)
    % output b rounded to whole hours
    r=round(t*24)/24;
end

