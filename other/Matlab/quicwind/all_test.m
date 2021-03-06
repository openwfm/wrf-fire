disp('running all tests')
err=[adj_mat_test,...
    fun2mat_test,...
    mlap3z_test,...
    adj_test,...
    mass_cons_int_test,...
    poisson_fft3z_test,...
    wind2flux_test,...
    mat_wind_flux_div_test,...
    mass_cons_flux_test
    ];
max_err=max(err)
if max_err < 1e-9, disp('all tests OK'), end
plot_mesh_test
block_example
