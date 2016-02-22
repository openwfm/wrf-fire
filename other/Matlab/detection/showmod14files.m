function showmod14files(search)
% display one or more MOD14 Matlab files
% input:
%   search  file name, or search string, default '*.mat'
if ~exist('search','var'),
    search='*.mat';
end
files=dir(search);
for i=1:length(files),
    file=files(i).name;
    v=readmod14(file);
    v.axis=[-130,-70,15,60];
    showmod14(v), drawnow
    pause(1)
    % hold on
end
hold off
