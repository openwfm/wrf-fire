function c=vec2cell(v,p)
[m,n]=size(p);
k=0;
for j=1:n
    for i=1:m
        s = size(p{i,j});
        e = prod(s);
        c{i,j}=reshape(v(k+1:k+e),s);
        k=k+e;
    end
end
if length(v)~=k,
    error('incompatible vector and cell template')
end
end