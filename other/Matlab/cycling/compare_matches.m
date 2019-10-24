function [ scores ] = compare_matches(wrf_single, wrf_cycle )
%functio gets comparison scores for two simulations
%inputs:

close all
scores(1) = match_detections(wrf_single);
scores(2) = match_detections(wrf_cycle);

figure(1), xs = xlim; ys = ylim;
figure(2), xc = xlim; yc = ylim;

x = [xs(:);xc(:)];
y = [ys(:);yc(:)];

x_lim = [min(x(:)) max(x(:))];
y_lim = [min(y(:)) max(y(:))];


figure(1), xlim(x_lim), ylim(y_lim)
figure(2), xlim(x_lim), ylim(y_lim)

end

