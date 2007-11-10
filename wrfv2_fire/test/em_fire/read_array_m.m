function a=read_array_m(f);
b=load(f);
l=length(b);
m=b(1);
n=b(2);
s=m*n+2;
fprintf(1,'matrix size %i %i from file %s length %i should be %i\n',m,n,f,l,s)
if l~=s,
    error('bad format')
end
a=reshape(b(3:m*n+2),[m,n]);
end