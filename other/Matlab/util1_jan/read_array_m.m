function a=read_array_m(f,num1,num2);
% a=read_array_m(f,num1,num2)
% read array produced by matching call write_array_m
% in module_fr_sfire_util.F
%
% a=read_array_m(f)
% read array from file f

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
fprintf(1,'matrix size %i:%i %i:%i %i:%i from file %s\n',its,ite,jts,jte,kts,kte,f)
if l~=s,
    error(sprintf('incorrect file length, should be %i',s))
end
a=reshape(b(7:s),[m,n,o]);
end
