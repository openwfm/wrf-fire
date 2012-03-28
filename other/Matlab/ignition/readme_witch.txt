% This code is done for the real case for the witch_fire


% 1) cd wrf-fire/WRFV3/run, start matlab, add ../../other/Matlab/ignition/ to your path
  2) Make sure your file, where your perimeter is specified lies in the same folder. 
     For the provided example cp  ../../other/Matlab/ignition/data.txt .
  3) First three rows of your perimeter file should be:
                1rt row - time_now (second number is not needed, is set to 0);
	            2nd row - size of the mesh;
	            3rd row - coordinates of ignition point;
	            All next rows - coordinates of all the
	            points on the boundary (lon,lat). 

  5) addpath ../../other/Matlab/util1_jan
     addpath ../../other/Matlab/netcdf
  6) In the command line run  
  data='data.txt'; // or another name, depends how your file that contains the perimeter is called.
  Run wrf to produce a wrfout file.
  wrf_out='wrfout_d01_2007-10-21_12:00:00'; // or if you use another wrf_out, that you have
  B=ignition7(data,wrf_out);

% Output:   Matrix of time of ignition made for wrf_input
Output lies in data_out1.txt




