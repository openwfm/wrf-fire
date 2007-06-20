function a=read_array(name)
% read matrix a from a given file name.txt
filename=[name,'.txt'];
if ~exist(filename,'file'),
    a=[];
    return
end
X=load(filename);
magicnumber=X(1,:);
if magicnumber~=456
    error(['cannot load matrix in file ',filename,' bad format'])
end
[m,n]=size(X);
if n~=1,
    error('file must contain column vector')
end
nd=X(2);
n1=X(3);
n2=X(4);
n3=X(5);
global read_array_msg
if ~isempty(read_array_msg)
    if read_array_msg,
        fprintf('reading matrix size %g %g %g from file %s\n',n1,n2,n3,filename)
    end
end
a=reshape(X(6:m),n1,n2,n3);
