function [ output ] = plot_slice( w,g )
%output = plot_slice(tign)
% function plots a vertical slice(s) of the fire arrival time cone
% inputs :
%   w - struct with tign_g, fxlat,fxlon
%   g - sturct with detection data
% outputs:
% nothin so far

%%% create list of fires
fires = [];
fire_lats = [];
fire_lons =[];
fire_times =[];
for i = 1:length(g)
    %fprintf('Granule %i \n',i)
    data = g(i).data >= 7;
    fire_lats = [fire_lats; g(i).xlat(data)];
    fire_lons = [fire_lons; g(i).xlon(data)];
    fire_times = [fire_times; g(i).time*ones(size(g(i).xlat(data)))];
end

% work with smaller domain
force = 1;
red = subset_domain(w,force);

[ig_lat,ig_lon] = find(red.tign_g == min(red.tign_g(:)));
ns = red.tign_g(:,ig_lon);
ew = red.tign_g(ig_lat,:);

figure
%plot(red_lat(2,:),ns)
plot(ns)
%axis([red.min_lat red.max_lat red.min_tign red.max_tign_g])
title('North-South slice through ignition point')





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%  block below was first attempot     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%plot slices through the ignition point, aligned NS and EW
% [ig_lat,ig_lon] = find(tign == min(tign(:)));
% mask = tign < max(tign(:));
% % [m,n] = find(mask);
% m = find(mask);
% m_min = min(m);
% m_max = max(m);
% % n_min = min(n);
% % n_max = max(n);
% red_tign = w.tign_g(m);
% red_lon = w.fxlong(m);
% red_lat = w.fxlat(m);
% lon_min = min(red_lon(:));
% lon_max = max(red_lon(:));
% lat_min = min(red_lat(:));
% lat_max = max(red_lat(:));
% t_min = min(tign(:));
% t_max = max(tign(:));
% bounds = [lon_min lon_max lat_min lat_max t_min t_max];
% 
% % figure, contour(tign(m_min:m_max,n_min:n_max));
%  
% ns = tign(:,ig_lon);
% ew = tign(ig_lat,:);
% 
% 
%figure
%plot(red_lat(2,:),ns)
%plot(ns)
%axis([lat_min lat_max t_min t_max])
%title('North-South slice through ignition point')
% 
% % figure
% % plot(red_lon(:,2),ew)
% % title('East-West slice through ignition point')
% % xlabel('Lon')
% % ylabel('Time')
% %hold on
% %pause




end

