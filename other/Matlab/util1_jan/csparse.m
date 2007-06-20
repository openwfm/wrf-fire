function [d,x,r,v]=csparse(a)
% convert matrix from ij to column sparse format
% in:
% a       matlab sparse matrix
% out:
% d       vector [number of rows; number of columns; number of nonzeros]
% x       index
% r       row indices
% v       entry values
% 
% row indices for column j are r(x(j):x(j+1)-1)
% values      for column j are v(x(j):x(j+1)-1)
 
[r,c,v]=find(a);
% make sure r c v are all columns
r=r(:);c=c(:);v=v(:);
[m,n]=size(a);
s=length(r);
d=[m;n;s];
[tmp,index]=sort(m*c+r);
c=c(index);
r=r(index);
v=v(index);
x=zeros(n+1,1);
x(1)=1;
oldcol=0;
for i=1:s
    newcol=c(i);
    if(newcol~=oldcol),
        x(oldcol+1:newcol)=i;
    end
    oldcol=newcol;
end
x(oldcol+1:n+1)=s+1;
