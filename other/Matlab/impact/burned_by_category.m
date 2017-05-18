function area=burned_by_category(t,burn_time)
% out=burned_by_category(fuel,t,utc)
% Input:
%    t          structure created from wrfout as follows:
%               t=read_wrfout_tign('wrfout file name')
%               save t, copy t.mat to another computer if needed, load t
%    burn_time  date string in a format understood by Matlab datenum function
%               assumed in UTC time zone
% Output:
%    area(k)  (m^2) area burned in category k until burn_time
%
% Usage:
%    first set search path:
%    clone the wrf-fire git repository
%    cd wrf-fire/other/Matlab
%    startup
%    cd to your working directory
%    load t
%    area=burned_by_category(t,'2011-06-29_00:00:00')


end_times=char(t.times(:,end)');
end_datenum = datenum(end_times); % in days from some instant in the past
end_minutes=t.xtime(end); % from simulation start
start_datenum=end_datenum-end_minutes/(24*60);
burn_datenum=datenum(burn_time);
burn_seconds=(burn_datenum-start_datenum)*24*60*60;
da=t.dx*t.dy/prod(size(t.fxlong)./size(t.xlong));
acre=4046.872609874252; % convert from m^2 to ac

fprintf('Simulation start %s\n',datestr(start_datenum));
fprintf('Simulation end   %s\n',datestr(end_datenum));
fprintf('Burn cut off     %s = %20g from sim start\n',datestr(burn_datenum));
fprintf('Fire mesh cell %g m^2\n',da);

cats = t.nfuel_cat .* (t.tign_g <= burn_seconds);
% now cats(i,i) is cat number if cell burned, 0 if not
num_cats=max(t.nfuel_cat(:));
for i=1:num_cats
    count(i)=sum(cats(:)==i);
end
area = count*da;
end
