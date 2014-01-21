function Y=dst2(X);
% Y=dst2(X)
% 2D discrete sine transform
Z=dst(X');
Y=dst(Z');

