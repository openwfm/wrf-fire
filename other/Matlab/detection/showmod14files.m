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
    showmod14(file)
    pause(2)
end
    
