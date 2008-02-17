function maxerr=compareto(d1)
f=dir(['*_0*.txt']);
n=length(f);
err=zeros(n);
for i=1:n
    name=f(i).name;
    try
        v=read_array_m([name]);
        v1=read_array_m([d1,'/',name]);
        diff=max(abs(v(:)-v1(:)));
        sz=(max(abs(v(:)))+max(abs(v1(:))))*0.5;
        rel=diff/sz;
        err(i)=rel;
        fprintf('%s diff %g rel %g\n',name,diff,rel)
    catch
    end
end
maxerr=max(err);
end