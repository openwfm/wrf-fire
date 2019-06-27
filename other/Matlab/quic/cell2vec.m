function v=cell2vec(c)
[m,n]=size(c);
v=[];
for j=1:n
    for i=1:m
        v=[v;c{i,j}(:)];
    end
end
end