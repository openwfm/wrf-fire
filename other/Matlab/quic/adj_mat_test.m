disp('creating matrix representation of div3 and grad3')
n=[2,2,2];
h=rand(1,3);
g=@(u)cell2vec(grad3z(u,h));
u=zeros(n);t=grad3z(u,h);  % size template
d=@(u)div3(vec2cell(u,t),h);
gm=fun2mat(g,n);  % gradient matrix
dm=fun2mat(d,[size(gm,1),1,1]);
err=big(dm+gm')