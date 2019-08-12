% function wind2flux_test
nx=5; ny=3; nz=4;
h=[rand,rand,1];
hh=rand(1,3);
% corner nodes
[x,y,z]   =ndgrid(hh(1)*[0:nx],hh(2)*[0:ny],hh(3)*[0:nz]);
% midpoints
[xm,ym,zm]=ndgrid(h(1)*([1:nx]-0.5),h(2)*([1:ny]-0.5),h(3)*([1:nz]-0.5));
X = {x,y,z};
% test field
% gradient for sizing
U = grad3z(xm,h,1);
fl=wind2flux(U,X);
% divergence just if it goes through
lapU=div3(fl,h);


c=[rand,rand,0];
for i=1:3,
    U{i} = 0*U{i}+c(i);
end
fl=wind2flux(U,X);
disp('constant horizontal wind, divergence should be zero')
d=div3(fl);
err=big(d)

c=[rand,rand,rand];
for i=1:3,
    U{i} = 0*U{i}+c(i);
end
fl=wind2flux(U,X);
disp('constant wind, divergence zero except at the bottom')
d=div3(fl);
err=big(d(:,:,2:end))








