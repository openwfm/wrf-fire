function u=poisson_fft3q(f,h)
% solve poisson equation with QUIC boundary conditions:
% lambda = 0 top and size, dlambda/dz = 0 on the bottom
n=size(f);
% reflect top to the bottom
r = zeros(n(1),n(2),2*n(3));
r(:,:,1:n(3))=f(:,:,n(3):-1:1);
r(:,:,n(3)+1:2*n(3))=f;
% solve with zero boundary conditions
u=poisson_fft3z(r,h);
% check up and down symmetry
err_sym = big(u(:,:,n(3):-1:1)-u(:,:,n(3)+1:2*n(3)))
% return upper half
u = u(:,:,n(3)+1:2*n(3));
