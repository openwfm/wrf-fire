% This code is done for the real case for the witch_fire

% 1) Put the files: ignition7.m; line_dist.m; line_sign.m in the folder
     wrf-fire/WRFV3/run
  2) Make sure your file, where your perimeter is specified and your wrfout file lie in the same folder. 

  3) Run Matlab
  
  4) Run B=ignition7('data.txt','wrfout_d01_2007-10-21_12:00:00_real_case');
  
  5) Matrix of time of ignition made for wrf_output will be printed to

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




