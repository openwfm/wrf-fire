function a=ijsparse(d,x,r,v)
% convert matrix from ij to column sparse format
% in:
% d       vector [number of rows; number of columns; number of nonzeros]
% x       index
% r       row indices
% v       entry values
% out:
% a       matlab sparse matrix
% 
% row indices for column j are r(x(j):x(j+1)-1)
% values      for column j are v(x(j):x(j+1)-1)

% matrix sizes
m=d(1);
n=d(2);
s=d(3);

% check input
if ~isvector(x),error('x must be vector'),end
if ~isvector(r),error('r must be vector'),end
if ~isvector(v),error('v must be vector'),end
if length(x)~=n+1,error('bad length of x'),end
if length(r)~=s,error('bad length of r'),end
if length(v)~=s,error('bad length of v'),end
if any(x~=round(x)) | any(x<1) | any(x>s+1) | x(n+1)-1~=s,
    error('bad values of x')
end
if any(r~=round(r)) | any(r<1) | any(r>m),
    error('bad values of r')
end

% create column index
c=zeros(s,1);
for j=1:n
    for k=x(j):x(j+1)-1
        c(k)=j;
    end
end
a=sparse(r,c,v,m,n);