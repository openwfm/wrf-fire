function err = mat_gen_wind_flux_div_test
% test mat_gen_wind_flux_div against mat_wind_flux_div

% set
%    mesh_len   vector size 3 of mesh dimensions
%    h          vector size 3 of mesh steps
mesh_len=[10,13,3];
%h=[0.1,0.2,0.05];
h=rand(1,3);
disp('mat_wind_flux_div_test')
% test mesh with a hill terrain (unit vectors for now)
X = uniform_mesh(mesh_len,h);
%X = uniform_mesh([mesh_len,mesh_len,mesh_len],[0.1,0.1,0.1]);
%X = add_terrain_to_mesh(X,'hill','shift',0.4);
X = add_terrain_to_mesh(X,'hill','squash',0.4);
% matrix of wind flux divergence
disp('Time for new matrix generation:')
tic
DM = mat_gen_wind_flux_div(X);
toc
disp('Time for old matrix generation:')
tic
DM_old = mat_wind_flux_div(X);
toc
disp('Compare both methods')
err = big(DM - DM_old);