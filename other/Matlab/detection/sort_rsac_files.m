function p=sort_rsac_files(prefix)
% d=sort_rsac_files(file_search)
% example;: log_likelihood('TIFs/','w.mat')
%
% in:
% file_search    directory search string
% d              cell array of file names ordered by time

% insert query to use tifs of Level2 data here
use_tifs = input_num('Use TIF files? yes = 1',1);

if use_tifs == 1
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
    
else
    dhdf=dir([prefix,'*.hdf']);
    dh5 = dir([prefix,'*.h5']);
    dnc = dir([prefix,'*.nc']);
    d=[{dhdf.name},{dh5.name},{dnc.name}];
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
    
    %check to make sure geolocation and data files are both present!
    if mod(nfiles,2) ~= 0
        fprintf('Mismatch, number of files is not an even number')
    end
    
    
    
end % if use_tifs
end