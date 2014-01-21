function Y = dst(X)
% Y = dst(X)
% apply sine transform to columns of X
[n,m] = size(X);
X1 = [ zeros(1,m); X ; zeros(n+1,m) ];
X2 = imag( fft(X1) );
Y = X2 (2:n+1,:);
