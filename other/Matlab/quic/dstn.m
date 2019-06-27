function Y = dstn(X,N)
% discrete sine transform along dimension N
% Y = dst(X,N)
% discrete sine transform along dimension N of X
n = size(X);
% permute dimensions N <-> 1
p = 1:length(n);
p(N)=1; p(1)=N;
Y = permute(X,p);
% pad dim 1 by zeros and collapse the othres
m = size(Y);
mm = prod(m)/m(1);
Y = reshape(Y,m(1),mm);
W = zeros(2*m(1)+2,mm);
W(2:m(1)+1,:)=Y;
Y = imag( fft(W,[],1) );
% pick transform between pads and uncollapse
Y = reshape(Y(2:m(1)+1,:),m);
% permute dimensions N <-> 1 back
Y = permute(Y,p);
