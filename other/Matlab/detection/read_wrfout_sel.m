function h=read_wrfout_sel(files,vars,nsamples)
% h=read_wrfout_sel(files,vars,nsamples)
% read selected time levels from wrfouts
% in 
%     files     cell array of file names
%     files     cell array of variable names; if followed by 'sparse' will be read as such
%     nsamples  number of timestep samples to read, if missing read all 
% out
%     h         structure with the selected variables

for i=1:length(files)
    [s,dims]=nc2struct(files{i},{'Times'},{});
    n(i)=dims.times(2);
end
nvar=0;
lastvar=0;
for i=1:length(vars)
    switch vars{i}
    case 'sparse'
        if ~lastvar,
            error('sparse flag must follow a variable name');
        end
        varsparse(nvar)=1;
        lastvar=0;
    otherwise
        lastvar=1;
        nvar=nvar+1;
        varname{nvar}=vars{i};
        spfield{nvar}=[lower(varname{nvar}),'_sparse'];
        varsparse(nvar)=0;
    end
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
    [f,dims]=nc2struct(files{k},varname,{},loc);
    if ~exist('h','var'),
        for jj=1:length(varname)
            field=lower(varname{jj});
            d=dims.(field);
            if varsparse(jj),
                h.(spfield{jj})=cell(1,nidx);
            else
                h.(field)=zeros([d(1:end-1),nidx]);
            end
        end
    end
    for jj=1:length(varname)
        if varsparse(jj),
            h.(spfield{jj}){jx}=sparse(f.(field));
        else
            field=lower(varname{jj});
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
end


