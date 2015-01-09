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
    load(file)
    s=file;
    showmod14(data,geotransform,s)
    pause(2)
end
    
