function y=mlap3(x,h)
% evaluate y=-x_11 - x_22 - x_33 on rectangular uniform grid
% assuming boundary values of x zero wrapped around
n=size(x);
xx=zeros(n+2);
xx(2:n(1)+1,2:n(2)+1,2:n(3)+1)=x;
mid1=2:n(1)+1;
mid2=2:n(2)+1;
mid3=2:n(3)+1;
y=(2*xx(mid1,mid2,mid3)-xx(mid1-1,mid2,mid3)-xx(mid1+1,mid2,mid3))/(h(1)*h(1))...
 +(2*xx(mid1,mid2,mid3)-xx(mid1,mid2-1,mid3)-xx(mid1,mid2+1,mid3))/(h(2)*h(2))...
 +(2*xx(mid1,mid2,mid3)-xx(mid1,mid2,mid3-1)-xx(mid1,mid2,mid3+1))/(h(3)*h(3));