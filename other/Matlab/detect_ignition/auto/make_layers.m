function [out_struct] = makeLayers(grid_nfo)
% makes layers to work with
% load grid_nfo.mat
% grid_nfo = 
%
%           pts: [49x2 double]
%      lat_done: [40.358 40.366 40.374 40.382]   % unneeded
%      lon_done: [-112.66 -112.65 -112.64 -112.63] % unneeded
%    lon_square: [7x7 double]
%    lat_square: [7x7 double]
%         names: {245x1 cell}
%         times: [5x1 double]
%          logs: [245x1 double]


name_spec = 'time = %d';
file_spec = 'run_%d';
kml_spec = 'run_%d.kml'
counter = 1;

for i=1:5
    layer(i).run = sprintf(file_spec,grid_nfo.times(i));
    layer(i).pts = grid_nfo.pts;
    layer(i).kml = sprintf(kml_spec,grid_nfo.times(i));
    layer(i).logs = grid_nfo.logs((i-1)*49+1:49*i,:);
    layer(i).names = grid_nfo.names((i-1)*49+1:49*i,:);
    layer(i).time =grid_nfo.times(i);
end
out_struct = layer;
end

