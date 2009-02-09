function [varargout]=read_array_sp(f,num1,num2)
% a=read_array_sp(f,num1,num2)
% [i,j,aij]=read_array_sp(f,num1,num2)
% [i,j,k,aijk]=read_array_sp(f,num1,num2)
% read array produced by matching call write_array_m
% as sparse, with correct indexing

% Jan Mandel, 2008

if exist('num1','var'),
    if ~exist('num2','var'),
        num2=-1;
    end
    f=[file_name(f,num1,num2),'.txt'];
end
b=load(f);
l=length(b);
its=b(1);
ite=b(2);
jts=b(3);
jte=b(4);
kts=b(5);
kte=b(6);
m=(ite-its+1);
n=(jte-jts+1);
o=(kte-kts+1);
s=m*n*o+6;
fprintf(1,'matrix size %i:%i %i:%i %i:%i from file %s length %i\n',its,ite,jts,jte,kts,kte,f,l)
if l~=s,
    error(sprintf('incorrect file length, should be %i',s))
end
if nargout==1,
    d=reshape(b(7:s),[m,n,o]);
    if(its>0 & jts > 0 & kts == 1 & kte == 1),
        a=sparse(ite,jte,m*n);
        a(its:ite,jts:jte)=d;
        varargout{1}=a;
    else
        error('incompatible dimensions for output as a sparse matrix')
    end
    return
end
ss=m*n*o;
i=zeros(ss,1);
j=zeros(ss,1);
k=zeros(ss,1);
a=zeros(ss,1);
for kk=0:kte-kts
    for jj=0:jte-jts
        for ii=0:ite-its
            idx=1+ii+m*jj+m*n*kk;
            i(idx)=its+ii;
            j(idx)=jts+jj;
            k(idx)=kts+kk;
            a(idx)=b(6+idx);
        end
    end
end
switch nargout
    case 3
        if kts == kte
            varargout{1}=i;
            varargout{2}=j;
            varargout{3}=a;
        else
            error('dimension 3 must be 1 for ouput as a 2d matrix')
        end
    case 4
            varargout{1}=i;
            varargout{2}=j;
            varargout{3}=k;
            varargout{4}=a;
    otherwise
        error('bad number of output arguments')
end
end
