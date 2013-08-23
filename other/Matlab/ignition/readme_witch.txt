% This code is done for the real case for the witch_fire
 Please see directions about how to run the code:

 A) 1) You just want to run witch example from the test file:
       Put the file test_ignition.m (it is located at wrf-fire/other/Matlab/ignition/) in the folder
       wrf-fire/WRFV3/run
    2) Run matlab
	3) Run test_ignition
	4) Output will be in data_out1.txt, created especially for wrfinput.

 
 B) You want to run the code manually by parts and play with coefficients:

  1a) Run matlab and go to the folder wrf-fire/WRFV3/run and then do
     addpath ../../other/Matlab/ignition
  OR

  1b) Put the files: ignition.m; line_dist.m; line_sign.m; read_file_ignition.m in the folder
     wrf-fire/WRFV3/run
  
  2) Make sure your file, where your perimeter is specified and your wrfout file lie in the same folder. 

  3) Run Matlab
  
  4) Run [unit_long,unit_lat,long,lat,time_now,mesh_size,ign_pnt,bound]=read_file_ignition('data.txt','wrfout_d01_2007-10-21_12:00:00_real_case');
  
  5) Run B=ignition(unit_long,unit_lat,long,lat,time_now,mesh_size,ign_pnt,bound);
  
  6) Matrix of time of ignition made for wrf_output will be printed to

      data_out1.txt
  
 6) Please use read_array_2d to interpret or graph results.


 

7) If you use different from my example perimeter data and wrfout, then
specify their names when you run ignition7.m
  
 First three rows of your perimeter file should be:
                 1rt row - time_now (second number is not needed, is set to 0);
                    2nd row - size of the mesh;
                    3rd row - coordinates of ignition point;
                    All next rows - coordinates of all the
                    points on the boundary (lon,lat).






