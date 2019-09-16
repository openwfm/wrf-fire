function err = mat_gen_wind_flux_div_test(mesh_len)
disp('mat_wind_flux_div_test')
% test mesh with a hill terrain (unit vectors for now)
X = uniform_mesh([mesh_len,mesh_len,mesh_len],[0.1,0.1,0.1]);
%X = uniform_mesh([mesh_len,mesh_len,mesh_len],[0.1,0.1,0.1]);
X = add_terrain_to_mesh(X,'hill','shift',0.4);
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