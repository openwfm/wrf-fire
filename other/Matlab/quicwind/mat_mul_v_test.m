function err = mat_mul_v_test
% test Mmul_v_test against mat_gen_wind_flux_div

disp('mat_mul_v_test')

mesh_len=[2,3,4];
h=rand(1,3);
nx = mesh_len(1);
ny = mesh_len(2);
nz = mesh_len(3);

% Test mesh with a hill terrain (unit vectors for now)
X = regular_mesh(mesh_len, h, 1.0);
X = add_terrain_to_mesh(X,'hill','squash',0.4);

% Initialize inputs for testing
testDT_vec = rand(prod(mesh_len),1);
testD_vec = rand((nx+1)*ny*nz + nx*(ny+1)*nz + nx*ny*(nz+1),1);
testA_vec = rand((nx+1)*ny*nz + nx*(ny+1)*nz + nx*ny*(nz+1),1);
testMT_vec = rand((nx+1)*ny*nz + nx*(ny+1)*nz + nx*ny*(nz+1),1);
testM_vec = rand((nx+1)*ny*nz + nx*(ny+1)*nz + nx*ny*(nz+1),1);

% Obtain sparse matrices for testing
D = mat_gen_wind_flux_div(X,'D');
M = mat_gen_wind_flux_div(X,'M');
A = sparse(diag(rand((nx+1)*ny*nz + nx*(ny+1)*nz + nx*ny*(nz+1),1)));

% Generate output vectors
resDT_mul = transpose(D) * testDT_vec;
resDT_loops = Dmul_v(X,'t',testDT_vec);

resD_mul = D * testD_vec;
resD_loops = Dmul_v(X,'n',testD_vec);

resA_mul = A * testA_vec;
resA_loops = Amul_v(X,A,testA_vec);

resM_mul = M * testM_vec;
resM_loops = Mmul_v(X,'n',testM_vec);

resMT_mul = transpose(M) * testMT_vec;
resMT_loops = Mmul_v(X,'t',testMT_vec);

disp('Compare M*v results')
err_Mv = big(resM_mul - resM_loops)

disp('Compare M^T*v results')
err_MTv = big(resMT_mul - resMT_loops)

disp('Compare A*v results')
err_Av = big(resA_mul - resA_loops)

disp('Compare D*v results')
err_Dv = big(resD_mul - resD_loops)

disp('Compare D^T*v results')
err_DTv = big(resDT_mul - resDT_loops)

errs = [err_Mv,err_MTv,err_Av,err_Dv,err_DTv];

err = max(errs);





