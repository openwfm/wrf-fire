% tests ellipse_3d function on a set of random data
clear

% s = [2 2];
% x = rand(13,1);
% y1 = normrnd(s(1).*x,1);
% y2 = normrnd(s(2).*x,1); 
a = 5;
y1 = a*rand(20,1);
y2 = a*rand(20,1);


data = [y1 y2];
ci = 1.74;

ellipse_3d(data,ci,1)