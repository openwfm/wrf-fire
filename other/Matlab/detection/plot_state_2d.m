function plot_state_2d(fig,red,s,tign,obs,time_now)

if obs(1).file(end) ~= 't'
    fprintf('L2 data, conflict with plot_state_2d.m \n')
    hold on
    for i=1;length(obs)
        mask = obs(i).data > 6;
        scatter(obs(i).lon(mask),obs(i).lat(mask),'r*')
    end
    hold off    
    return
end

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
if ~iscell(s),
    s={s};
end

color={'k','b','-.r'};
for i=1:length(tign)
    [c,h]=contour(red.fxlong,red.fxlat,tign{i},[time_now,time_now],color{i});
    set(h,'linewidth',2)
end
h_legend=legend(s{:});
set(h_legend,'FontSize',16);
a=[red.min_lon,red.max_lon,red.min_lat,red.max_lat];
axis manual
axis(a)
grid on
hold off
end