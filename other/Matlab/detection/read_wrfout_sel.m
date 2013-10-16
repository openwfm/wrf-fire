function h=read_wrfout_sel(files,vars,nsamples)
% h=readsel_wfout(files,vars)
% read selected time levels from wrfouts
% in 
%     files     cell array of file names
%     files     cell array of variable names
%     nsamples  number of samples to read 
% out
%     h         structure with the selected variables

for i=1:length(files)
    [s,dims]=nc2struct(files{i},{'Times'},{});
    n(i)=dims.times(2);
end
ntot=sum(n);
if ~exist('nsamples','var') | nsamples == 0,
    step=1;
else
    step = floor((ntot+nsamples-1)/nsamples)
end
start=[1,cumsum(n)+1]
for j=mod(ntot-1,step)+1:step:ntot
    k=find(j>=start);k=k(end);
    loc=j-start(k)+1;
    fprintf('reading step %i as %i from file %i\n',j,loc,k)
    if loc <=0 | k > length(files), error('bad index'), end
    [f,dims]=nc2struct(files{k},vars,{},loc);
    if ~exist('h','var'),
        h=f;
    else
        for j=1:length(vars)
            field=lower(vars{j});
            n=length(dims.(field));
            h.(field)=cat(n,h.(field),f.(field));
        end
    end
end


