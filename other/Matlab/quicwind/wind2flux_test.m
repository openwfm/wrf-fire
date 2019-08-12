% function wind2flux_test
nx=2; ny=2; nz=2;
h=[1,1,1];
hh=rand(1,3);
% corner nodes
[x,y,z]   =ndgrid(hh(1)*[0:nx],hh(2)*[0:ny],hh(3)*[0:nz]);
% midpoints
[xm,ym,zm]=ndgrid(h(1)*([1:nx]-0.5),h(2)*([1:ny]-0.5),h(3)*([1:nz]-0.5));
X = {x,y,z};
% test field
c=rand(1,4);
f = c(1)*xm + c(2)*ym + c(3)*zm;
% its gradient
U = grad3z(f,h,1);
fl=wind2flux(U,X);
% divergence 
lapU=div3(fl,h);











