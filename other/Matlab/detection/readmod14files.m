function r=readmod14files(file_search,ax)
% r=readmod14files(file_search,ax)
%   file_search   search string
%   ax            bounds [min_lon, max_lon, min_lat, max_lat]

% find the files
d=dir(file_search);d={d.name};
if(isempty(d)), error(['No files found for ',file_search]),end
prefix=regexp(file_search,'^.*/','match');  % all up to the last /
if isempty(prefix),  % comes as cell, convert to string
    prefix='';
else
    prefix=prefix{1};
end

% order the files in time
nfiles=length(d);
t=zeros(1,nfiles);
for i=1:nfiles
    t(i)=rsac2time(d{i});
end
[t,i]=sort(t);
d={d{i}};

k=0;
for i=1:nfiles,
    file=d{i};
    v=readmod14([prefix,file]);
     % select fire detection within the domain
    xj=find(v.lon >= ax(1) & v.lon <= ax(2));
    xi=find(v.lat >= ax(3) & v.lat <= ax(4));
    if ~isempty(xi) & ~isempty(xj)
        k=k+1;
        v.data=v.data(xi,xj);    % subset data
        v.lon=v.lon(xj);
        v.lat=v.lat(xi);
        r.time(k)=t(i);
        v.axis=ax;
        r.x{k}=v;
    end
end
end