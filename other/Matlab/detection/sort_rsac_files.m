function d=sort_rsac_files(file_search)
% d=sort_rsac_files(file_search)
%
% file_search    directory search string
% d              cell array of file names ordered by time

d=dir(file_search);d={d.name};
if(isempty(d)), error(['No files found for ',file_search]),end

% order the files in time
nfiles=length(d);
t=zeros(1,nfiles);
for i=1:nfiles
    t(i)=rsac2time(d{i});
end
[t,i]=sort(t);
d={d{i}};

end