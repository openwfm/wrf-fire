function write_array(a,name)
% read matrix a from a given file name.txt
if ndims(a)>3,
    error('array must have at most 3 dimensions')
end
filename=[name,'.txt'];
X=zeros(5,1);
X(1)=456; % magic number
X(2)=3;
X(3)=size(a,1);
X(4)=size(a,2);
X(5)=size(a,3);
X=[X;a(:)];
fprintf('writing matrix size %g %g %g to file %s\n',...
    X(3:5),filename)
h=fopen(filename,'w');
fprintf(h,'%.17g\n',X);
fclose(h);
