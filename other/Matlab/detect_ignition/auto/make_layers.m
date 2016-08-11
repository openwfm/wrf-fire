function [out_struct] = makeLayers(grid_nfo)
% makes layers to work with
%load grid_nfo.mat


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

