function DM=mat_wind_flux_div(X)
% get matrix of flux divergencen
% input:
%     X   mesh
% output:
%     DM  matrix

check_mesh(X);
wind_template=grad3z(rand(size(X{1})-1),[1 1 1]);  % cell matrix with entries size of u,v,w
n = sum(cellfun(@numel,wind_template));   % size of vector this will act on
Mfun=@(u)cell2vec(wind2flux(vec2cell(u,wind_template),X)); 
Mmat=fun2mat(Mfun,[n,1,1]);
Dfun=@(u)div3(vec2cell(u,wind_template));
Dmat=fun2mat(Dfun,[n,1,1]);
DM = Dmat * Mmat;
DM=Dmat*Mmat;
end