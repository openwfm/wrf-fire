function h=read_wrfout_sel(files,vars,nsamples)
% h=read_wrfout_sel(files,vars,nsamples)
% read selected time levels from wrfouts
% in 
%     files     cell array of file names
%     files     cell array of variable names
%     nsamples  number of timestep samples to read, if missing read all 
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
idx=mod(ntot-1,step)+1:step:ntot;
nidx=length(idx);
for jx=1:nidx
    j=idx(jx);
    k=find(j>=start);k=k(end);
    loc=j-start(k)+1;
    fprintf('reading step %i as %i from file %i into %i\n',j,loc,k,jx)
    if loc <=0 | k > length(files), error('bad index'), end
    [f,dims]=nc2struct(files{k},vars,{},loc);
    if ~exist('h','var'),
        for jj=1:length(vars)
            field=lower(vars{jj});
            d=dims.(field);
            h.(field)=zeros([d(1:end-1),ntot]);
        end
    end
    for j=1:length(vars)
        field=lower(vars{j});
        n=length(dims.(field));
        switch n
            case 2
                h.(field)(:,jx)=f.(field);
            case 3
                h.(field)(:,:,jx)=f.(field);
            case 4
                h.(field)(:,:,:,jx)=f.(field);
            otherwise
                error(['unsupported number of dimensions ',num2str(n)]);
        end
    end
end


