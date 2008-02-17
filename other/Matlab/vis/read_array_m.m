function a=read_array_m(f);
b=load(f);
l=length(b);
m=b(1);
n=b(2);
o=b(3);
s=m*n*o+3;
fprintf(1,'matrix size %i %i %i from file %s length %i should be %i\n',...
    m,n,o,f,l,s)
if l~=s,
    error('bad format')
end
a=reshape(b(4:s),[m,n,o]);
end