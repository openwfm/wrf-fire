function s=size1(a,ndim)
% s=size1(a,ndim)
% return size(a) extended by 1 to ndim dimensions
s=size(a);
s=[s,ones(1,ndim-length(s))];
end 
