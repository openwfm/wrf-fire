function maxerr=compareto(d1,pat)
if ~exist('pat','var'),pat=['*_0*.txt'],end
f=dir(pat);
n=length(f);
err=zeros(n);
for i=1:n
    name=f(i).name;
    try
        v=read_array_m([name]);
        v1=read_array_m([d1,'/',name]);
        vdiff=v(:)-v1(:);
        sz=(max(abs(v(:)))+max(abs(v1(:))))*0.5;
        maxadiff=max(abs(vdiff));
        meandiff=mean(vdiff);
        stddiff=std(vdiff);
        reladiff=maxadiff/sz;
        err(i)=reladiff;
        fprintf('%s diff: max abs %g mean %g std %g max abs rel %g\n',...
            name,maxadiff,meandiff,stddiff,reladiff)
    catch
        disp(['comparison of ',name,' failed'])
    end
end
maxerr=max(err);
end