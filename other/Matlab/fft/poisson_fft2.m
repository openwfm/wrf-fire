function U = poisson_fft2(F,h)
n = size(F);
% eigenvalue terms
X=poisson_1d_eig(n(1),h(1));
Y=poisson_1d_eig(n(2),h(2));
% VV=(2/(n+1)*ones(n,n)./(V'(ones(1,n)+ones(1,n)*V');
U=dst2(F);
for j=1:n(2)
    for i=1:n(1)
        U(i,j)=U(i,j)/(X(i)+Y(j));
    end
end
U=dst2(U)*4/((n(1)+1)*(n(2)+1)); % scale for nonunitary DST2



