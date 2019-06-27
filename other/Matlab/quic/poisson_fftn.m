function U = poisson_fftn(F,h)
% solve poisson equation in nD, n up to 3 supported
d = length(h);
n = ones(1,3);
n(1:ndims(F)) = size(F);
U = F;
X={0,0,0};
for i=1:d
    X{i}=poisson_1d_eig(n(i),h(i));
    U=dstn(U,i);
end
for i3=1:n(3)
    for i2=1:n(2)
        for i1=1:n(1)
            U(i1,i2,i3)=U(i1,i2,i3)/(X{1}(i1)+X{2}(i2)+X{3}(i3));
        end
    end
end
for i=1:d
    U=dstn(U,i);
end
U=U*(2^d)/prod(n(1:d)+1); % scale for nonunitary DST



