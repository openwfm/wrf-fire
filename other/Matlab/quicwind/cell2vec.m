function v=cell2vec(c)
% v=cell2vec(c)
% Flatten a cell array of matrices to a vector

[m,n]=size(c);
v=[];
t=sum(sum(cellfun(@numel,c)));
v=zeros(t,1);
k=0;
for j=1:n
    for i=1:m
        s = size(c{i,j});
        e = prod(s);
        v(k+1:k+e)=reshape(c{i,j},[e,1]);
        k=k+e;
    end
end
if t~=k,
    error('internal')
end
end