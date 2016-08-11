function [ out_struct ] = log_struct( in_struct )
%out_struct = log_struct(in_struct)
%Outputs a structure array for use in plotting data with countour3
% Use the make_layers.m function to split big variable 'layers' into
% individual ignition times
%
% Inputs - in_struct
%     structure array with the following fields
%       pts - nx2 array with [lat lon] as rows
%       logs - nx1 array with log-likelihoods of the pts
%       names - nx1 cell array with names for the pts
%       run - string with name of the data run
%       time - scalar with ignition time in seconds after simulation
% Output - out struct
%    structure array with the following fields
%       lon - 4x4 array with columns of lon from pts
%       lat - 4x4 array with rows of lat from pts
%       log - 4x4 array with log likelihoods
%       time - scalar with ignition time in seconds after simulation

out_struct.time = in_struct.time;
out_struct.lon = reshape(in_struct.pts(:,1), [7 7])';
out_struct.lat = reshape(in_struct.pts(:,2), [7 7])';
out_struct.log = reshape(in_struct.logs,[7 7])';

% figure
% contour3(out_struct.lon,out_struct.lat,out_struct.log,50)
% title('Contour plot of log-likelihoods');
% xlabel('lon')
% ylabel('lat')
% zlabel('log-likelihood')
% %figure
% %mesh(out_struct.lon,out_struct.lat,out_struct.log)
% figure
% surfc(out_struct.lon,out_struct.lat,out_struct.log);
% xlabel('lon');
% ylabel('lat');
% zlabel('log-likelihood')
figure;
[C,h] = contour(out_struct.lon,out_struct.lat,out_struct.log,linspace(-61000,-31000,31));
clabel(C,h,'FontSize',8,'LabelSpacing',200);
set(h,'LineWidth',1.5);
str = sprintf('Contours of log-likelihoods, time = %d',in_struct.time);
title(str);
xlabel('lon');
ylabel('lat');
colormap hsv;
end

