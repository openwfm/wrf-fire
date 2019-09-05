function outplut = plot_contours(w)
%

%contourf(w.fxlong,w.fxlat,w.tign_g)

% find reduced domain
red = subset_domain(w)

prefix = '../TIFs/';
p=sort_rsac_files(prefix);
time_bounds=subset_detection_time(red,p);
%fig = 1;
%change to compute g
%g = load_subset_detections(prefix,p,red,time_bounds);
load('g.mat');

num_contours = 6;
contour_lines = linspace(red.start_time,red.end_time,num_contours);

contourf(red.fxlong,red.fxlat,red.tign_g,contour_lines)
colorbar
xlabel('Lon (degrees)')
ylabel('Lat (degrees)')


end
