function err=fun2mat_test
disp('fun2mat_test')
h=rand(1,3);
n=[2,2,3];
fun = @(u)cell2vec(grad3z(u,h));
g=fun2mat(fun,n);
l=rand(n);
err=full(big(cell2vec(grad3z(l,h))-g*l(:)))