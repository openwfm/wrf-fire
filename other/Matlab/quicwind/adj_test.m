function adj_test
% desting if grad3z and div3 are adjoint
h=rand(1,3);
n=[20,7,2];
lambda = rand(n);
g = grad3z(lambda,h);
for i=1:length(g)
    u{i}=rand(size(g{i}));
end
div3u = div3(u,h);
l3z = grad3z(lambda,h);
err_adj = aprod(lambda,div3u) + aprod3(l3z,u)
end

function a=aprod(x,y)
a = dot(x(:),y(:));
end

function a=aprod3(x,y)
a = aprod(x{1},y{1})+aprod(x{2},y{2})+aprod(x{3},y{3});
end