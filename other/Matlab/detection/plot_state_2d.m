function plot_detect(fig,red,s,tign,obs,time_now)
figure(fig),clf
for i=1:length(obs)
    x=obs(i);
    kk=find(x.data(:)>=7);
    if ~isempty(kk),
            x.data=double(x.data);
            x.data(x.data<7)=NaN;
            x.data=x.data+3*floor((time_now-x.time)/0.25);
            showmod14(x,0.5)
            hold on
    end
end
if ~iscell(tign),
    tign={tign};
end
color={'--k','r','b'};
for i=1:length(tign)
    [c,h]=contour(red.fxlong,red.fxlat,tign{i},[time_now,time_now],color{i});
    set(h,'linewidth',2)
end
a=[red.min_lon,red.max_lon,red.min_lat,red.max_lat];
axis manual
axis(a)
hold off
title(s)
end