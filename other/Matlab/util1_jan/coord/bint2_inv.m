function [x1,x2]=bint2_inv(a1,a2,v1,v2)
% inverse bilinear interpolation
% [x1,x2]=bint2_inv(a1,a2,v1,v2)
% find x1 x2 such that v1=bint2(a1,x1,x2) and v2=bint2(a2,x1,x2)
% it is assumed that the mapping M given by 
% M: [i1,i2] -> {a1(i1,i2),a2(i1,i2)] is reasonably close to linear
% Jan Mandel, February 2006

[m,n]=size(a1);

% jacobian D at midpoint
d11 = (a1(m,1)-a1(1,1)+a1(m,n)-a1(1,n))/(2*(m-1));
d21 = (a2(m,1)-a2(1,1)+a2(m,n)-a2(1,n))/(2*(m-1));
d12 = (a1(1,n)-a1(1,1)+a1(m,n)-a1(m,1))/(2*(n-1));
d22 = (a2(1,n)-a2(1,1)+a2(m,n)-a2(m,1))/(2*(n-1));
detj=d11*d22-d12*d21;

% start from midpoint: 
x1=(1+m)/2;
x2=(1+n)/2;

maxit=20;
tol=1e-10;
for it=1:maxit
    % iteration: D*(xnew-x)=v-bint(x);
    res1=v1-bint2(a1,x1,x2);
    res2=v2-bint2(a2,x1,x2);
    err=max(abs(res1),abs(res2));
    x1=x1+(d22*res1-d12*res2)/detj;
    x2=x2+(-d21*res1+d11*res2)/detj;
    it,x1,x2,err
    if err<tol,
        break
    end
end
