function p=sort_rsac_files(prefix)
% d=sort_rsac_files(file_search)
% example;: log_likelihood('TIFs/','w.mat')
% 
% in:
% file_search    directory search string
% d              cell array of file names ordered by time

d=dir([prefix,'*.tif.mat']);d={d.name};
if(isempty(d)), error(['No files found for ',prefix]),end

% order the files in time
nfiles=length(d);
t=zeros(1,nfiles);
for i=1:nfiles
    f{i}=[prefix,d{i}];
    t(i)=rsac2time(d{i});
end
[t,i]=sort(t);
p.file={d{i}};
p.time=t;
end