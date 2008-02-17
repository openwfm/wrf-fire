function maxerr=compareto(d1)
f=dir(['*_0*.txt']);
n=length(f);
err=zeros(n);
for i=1:n
    name=f(i).name;
    try
        v=read_array_m([name]);
        v1=read_array_m([d1,'/',name]);
        vdiff=abs(v(:)-v1(:));
        sz=(max(abs(v(:)))+max(abs(v1(:))))*0.5;
        maxdiff=max(vdiff);
        meandiff=mean(vdiff);
        stddiff=std(vdiff);
        reldiff=maxdiff/sz;
        err(i)=reldiff;
        fprintf('%s diff max %g mean %g std %g max rel %g\n',...
            name,maxdiff,meandiff,stddiff,reldiff)
    catch
    end
end
maxerr=max(err);
end