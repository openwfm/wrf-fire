function v=bint2(a,x,y)
% v=interp_b2d(a,x,y)
% bilinear interpolation from a to position x,y
% Jan Mandel, February 2006

[m,n]=size(a);
if(x<1 | x>m | y<1 | y > n),
    error('arg out of range')
end
% map to lower left corner
i=min(floor(x),m-1);
j=min(floor(y),n-1);
t=x-i;
u=y-j;
v=a(i,j)*(1-t)*(1-u)+a(i+1,j)*t*(1-u)+a(i,j+1)*(1-t)*u+a(i+1,j+1)*t*u;
%err=v-interp2(a',x,y)
end
