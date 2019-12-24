function err=adj_mat_flux_test
disp('adj_mat_flux_test')
% disp('creating matrix representation of f(U)=div3(wind2flux(U))')
n=[2,2,2];
h=[1,1,1];

t=grad3z(zeros(n),h);  % wind size template
d=@(u)div3(vec2cell(u,t),h);
g=@(u)cell2vec(grad3z(u,h));
gm=fun2mat(g,n);  % gradient matrix
dm=fun2mat(d,[size(gm,1),1,1]);
err=big(dm+gm')
