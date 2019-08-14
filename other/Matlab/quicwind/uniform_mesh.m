function X=uniform_mesh(n,h)
% 
[x,y,z]   =ndgrid(h(1)*[0:n(1)],h(2)*[0:n(2)],h(3)*[0:n(3)]);
X = {x,y,z};
