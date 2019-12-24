Mass-consistent approximation as in WindNinja or QUIC
implemented by FFT

Jan Mandel, June 2019

Location: wrf-fire/other/Matlab/quic


Given wind field u0, the code minimizes ||u-u0|| subject to div u=0 with 
boundary condition u=0 on the ground.

The code uses  staggered grid for wind components at mid sides, similarly
as in WRF. The Lagrange multiplier lambda enforcing div u = 0 is at 
cell centers. The method consists of
1. preprocessing u0 and the mesh to set up the Laplace equation for lambda
2. solve the Laplace equation for lamda
3. postprocess lambda to compute u

FFT-based solver - Limited to uniform rectangular grid

The solution time scales as n log n with the size n of each dimension. 
The block example on 500x500x50 mesh takes about 5 seconds on macbook pro.
The FFT use is suboptimal due to matlab limitations. The code uses complex 
Fourier transform and mirroring in vertical direction. Standard
implementation by calls to appropriate real transforms should be 5 to 10 
times faster. Terrain implemented approximately by setting zero input 
wind u0 inside obstacles as in block_example.m.

File            Description

startup         
all_test        
block_example   

Function                    Purpose                                                         Test

Amul_v                      Multiply 3d field by a diagonal vector using 1d indexing        mat_mul_v_test
Dmul_v                      Divergence (or transpose) of 3d field using 1d indexing         mat_mul_v_test
Mmul_v                      Multiply wind field to flux matrix and 3d field, 1d indexing    mat_mul_v_test
add_terrain_to_mesh         Modify 3D mesh vertically to follow terrain                     plot_mesh_test
adj_mat_test                Test that the matrices of div3 and grad3z are adjoint            
adj_test.m                  Test if grad3z and div3 are adjoint, without matrices
all_test.m                  Run all tests
block_example.m             Flow with block obstacle, by FFT
cell2vec.m
cell_sizes.m
check_mesh.m
div3.m
dstn.m
flat.m
fun2mat.m
fun2mat_sym.m
fun2mat_test.m
grad3z.m
mass_cons_flux.m
mass_cons_flux_test.m
mass_cons_int.m
mass_cons_int_test.m
masscons.m
mat_div3.m
mat_flux.m
mat_gen_wind_flux_div.m
mat_gen_wind_flux_div_test.m
mat_mul_v_test.m
mat_wind_flux_div.m
mat_wind_flux_div_test.m
mlap3z.m
mlap3z_test.m
multigrid.m
multigrid_2d_test.m
multigrid_3d_test.m
plot_mesh.m
plot_mesh_test.m
plot_wind.m
plot_wind_above_terrain.m
poisson_fft3q.m
poisson_fft3z.m
poisson_fft3z_test.m
prolongation_2d.m
prolongation_3d.m
readme.m
regular_mesh.m
restriction_2d.m
restriction_3d.m
skew.m
startup.m                       Run this first to set up the environment.
uniform_mesh.m
vec2cell.m
wind2flux.m
wind2flux_test.m
wind2flux_trans.m
