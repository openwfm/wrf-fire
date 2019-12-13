function c=vec2cell(v,p)
% c=vec2cell(v,p)
% Reshape vector v into cell array of nd-arrays
% of the same sizes as the template p. The total number of entries
% in the vector v must equal to the total number of entries
% in the arrays in p
%
% in:
% v     vector
% p     cell array
% out:
% c     cell array
%

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
