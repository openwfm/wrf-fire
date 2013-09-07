function coarse_ind=map_submesh(fine_ind,coarse_size,fine_size)
% coarse_sub=map_submesh(fine_ind,size(coarse,size(fine))
% 
% fine maps to rectangular blocs of fine
% given index vector to fine find corresponding index vector to coarse 

% to test:
%   fine=kron(coarse,ones(m,n))
%   now fine(fine_ind) == coarse(coarse_ind)

ratio = fine_size ./coarse_size;
[fine_i,fine_j]=ind2sub(fine_size,fine_ind); % convert index to subscript
coarse_i = 1 + floor((fine_i - 1)/ ratio(1));
coarse_j = 1 + floor((fine_j - 1)/ ratio(2));
coarse_ind = sub2ind(coarse_size,coarse_i,coarse_j);

end