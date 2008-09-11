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
m=b(1);
n=b(2);
o=b(3);
s=m*n*o+3;
fprintf(1,'matrix size %i %i %i from file %s length %i\n',m,n,o,f,l)
if l~=s,
    error(sprintf('incorrect file length, should be %i',s))
end
a=reshape(b(4:s),[m,n,o]);
end