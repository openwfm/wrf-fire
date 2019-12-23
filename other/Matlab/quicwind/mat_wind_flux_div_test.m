function err = mat_wind_flux_div_test
disp('mat_wind_flux_div_test')
% test mesh with a hill terrain
% X = uniform_mesh([2,2,2],[.5,.5,.5]);
X = regular_mesh([3,4,5],[0.1,0.1,0.5],1.2);
X = add_terrain_to_mesh(X,'hill','squash',0.4);
% matrix of wind flux divergence
DM = mat_wind_flux_div(X);
% random test wind
U = grad3z(rand(size(X{1})-1),[1 1 1]); 
% its flux divergence on the mesh
L = div3(wind2flux(U,X));
disp('compare with matrix-vector multiplication')
Uvec = cell2vec(U);
Lvec = L(:);
err1 = big(DM * Uvec - Lvec);

wind_template=grad3z(rand(size(X{1})-1),[1 1 1]);  % cell matrix with entries size of u,v,w
n = sum(cellfun(@numel,wind_template));   % size of vector this will act on

disp('compare M^T with matrix-vector multiplication')
M = mat_wind_flux_div(X,'M');
MTu = wind2flux_trans(U,X);
err2 = big(M'*Uvec-cell2vec(MTu));

err = max(err1,err2);


