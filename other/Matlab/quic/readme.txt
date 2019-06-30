Mass-consistent approximation as in WindNinja or QUIC
implemented by FFT

Jan Mandel, June 2019

Location: wrf-fire/other/Matlab/quic

File            Description

startup         Run this first to set up the environment.
block_example   A simple example with block obstacle in the middle.

Given wind field u0, the code minimizes ||u-u0|| subject to div u=0 with 
boundary condition u=0 on the ground.

The code uses  staggered grid for wind components at mid sides, similarly
as in WRF. The lagrange multiplier is at cell centers.

The solution time scales as n log n with the size n of each dimension. 
The block example on 500x500x50 mesh takes about 5 seconds on macbook pro.

The FFT use is suboptimal due to matlab limitations. The code uses complex 
Fourier transform and mirroring in vertical direction. Standard
implementation by calls to appropriate real transforms should be 5 to 10 
times faster. 

The code should parallelize well in WRF using FFT similarly as "recursive
filters" in WRFDA. We may want to call FFTW instead of WRF supplied
FFTPACK. 

Terrain height is not implemented yet, set zero input wind u0 inside 
obstacles as in block_example.m.
