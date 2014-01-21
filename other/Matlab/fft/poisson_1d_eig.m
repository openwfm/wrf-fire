function X=poisson_1d_eig(n,h);
% X=poisson_1d_eig(n,h)
% eigenvalues of d^2/dx^2 with zero b.c. mesh 1:n stepsize h

X=4*(sin([1:n]*pi/(2*(n+1)))/h).^2;

end