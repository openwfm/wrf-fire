function [] = fire_ellipse( x )
% function takes a struct from the patch2 simulation and finds detections
% in x.data then sends them to the ellipse_3d which plots the cone
% describing the history of the fire

% load the path_fire_layer.mat file to have the struct x which is a set of
% detections for the Patch fire.

[rows cols] = find(x.data > 6 );
detects = [x.lon(cols) ; x.lat(rows)]';
ellipse_3d(detects,1.5,0)

end

