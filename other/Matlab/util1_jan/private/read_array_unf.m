function a=read_array_unf(file)
% read array from fortran unformatted file
fid=ftopen(file);
if fid < 0, error('cannot open file'),end
m=read_rec(fid,1,'int');
if m~=1, error('bad file format type'),end
n=read_rec(fid,1,'int');
if n>3, error('only 3 dimensions supported'),end
nn=[1,1,1];
for i=1:n
	nn(i)=read_rec(fid,1,'int');
end
fprintf(1,'reading dense matrix size %i %i %i from file %s\n',m,n,file)
a=zeros(nn);
for i3=1:nn(3)
	for i2=1:nn(2)
		a(:,i2,i3)=read_rec(fid,nn(1),'double');
	end
end
close(fid)
return
