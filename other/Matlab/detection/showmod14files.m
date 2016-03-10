function M=showmod14files(search)
% display one or more MOD14 Matlab files
% input:
%   search  file name, or search string, default '*.mat'
if ~exist('search','var'),
    search='*.tif.mat';
end

d=sort_rsac_files(search);

lonlat=[-130 -70 20 60];
for i=1:length(d),
    file=d{i};
    hold off
    v=readmod14(file);
    v.axis=lonlat;
    newplot
    showmod14(v)
    axis(v.axis)
    daspect([1,cos(0.5*sum(lonlat(3:4))*pi/180),1]);
    drawnow
    M(i)=getframe(gcf);
    % pause(1)
    % hold on
end
hold off
end