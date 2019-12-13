function err = mat_wind_flux_div_test
disp('mat_wind_flux_div_test')
% test mesh with a hill terrain
X = uniform_mesh([3,3,3],[0.1,0.1,0.5]);
%X = regular_mesh([3,3,3],[0.1,0.1,0.5],1.2);
X = add_terrain_to_mesh(X,'hill','squash',0.4);
% matrix of wind flux divergence
DM = mat_wind_flux_div(X);
% random test wind
U = grad3z(rand(size(X{1})-1),[1 1 1]); 
% its flux divergence on the mesh
L = div3(wind2flux(U,X)); 
disp('compare with matrix-vector multiplication')
Uvec = cell2vec(U);
DM * Uvec
Lvec = L(:);
err = big(DM * Uvec - Lvec)
