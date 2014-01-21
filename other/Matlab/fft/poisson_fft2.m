function U = poisson_fft2(F,h,power)
% U = poisson_fft2(F,h,power)
% multiply grid function on a rectangle by power of minus Laplacian
% with zero boundary conditions, using the sine Fourier series
% input:
%    F        matrix, values of the function on rectangle
%    h        mesh spacing
%    power    the desired power of the laplacian
% output
%    U        (-laplace)^power (F)

n = size(F);
% eigenvalue terms
X=poisson_1d_eig(n(1),h(1));
Y=poisson_1d_eig(n(2),h(2));
% VV=(2/(n+1)*ones(n,n)./(V'(ones(1,n)+ones(1,n)*V');
U=dst2(F);
for j=1:n(2)
    for i=1:n(1)
        U(i,j)=U(i,j)/((X(i)+Y(j))^power);
    end
end
U=dst2(U)*4/((n(1)+1)*(n(2)+1)); % scale for nonunitary DST2



