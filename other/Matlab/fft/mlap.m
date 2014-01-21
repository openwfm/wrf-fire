function y=mlap(x,h)
% evaluate y=-x_11 - x_22 on rectangular uniform grid
% it is assumed that boundary values of x are zero
[m0,n0]=size(x);
xx=zeros(m0+2,n0+2);
xx(2:m0+1,2:n0+1)=x;
[m,n]=size(xx);
y=zeros(m0,n0);
mid1=2:m-1;
mid2=2:n-1;
y=(2*xx(mid1,mid2)-xx(mid1-1,mid2)-xx(mid1+1,mid2))/(h(1)*h(1))...
  +(2*xx(mid1,mid2)-xx(mid1,mid2-1)-xx(mid1,mid2+1))/(h(2)*h(2));