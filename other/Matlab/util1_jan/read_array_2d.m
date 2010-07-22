function a=read_array_2d(filename)
% a=rad_arrray_2d(filename)
% read 2d matrix written by write_array_2d
d=load(filename);
m=d(1);
n=d(2);
if length(d)~=m*n+2,
    error('wrong number of terms in the file')
end
a=reshape(d(3:end),n,m)';
end

