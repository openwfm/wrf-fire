function a=fun2mat(fun,n)
% return matrix a such that a*u(:) = (fun(u))(:) for 3d u size n
u = zeros(n);
outn = size(fun(u));% size of output
a = sparse([],[],[],prod(outn),prod(n),5*prod(n));
k=0;
for i3=1:n(3)
    for i2=1:n(2)
        for i1=1:n(1)
            u(i1,i2,i3)=1;
            k=k+1;
            out = fun(u);
            a(:,k)=out(:);
            u(i1,i2,i3)=0;
        end
    end
end
            