% This is the code that computes the level set function based on its given perimeter and 
  time of ignitions of the points on the perimeter
% This code is done for the real case for the witch_fire
 Please see directions about how to run the code:

 A) 1) You just want to run witch example from the test file:
       Put the file main_function.m (it is located at wrf-fire/other/Matlab/ignition/) in the folder
       wrf-fire/WRFV3/run
	2) Run the wrf code for some time, so that you have any wrfout file   
    3) Edit main_function.m, and insert the name of your wrfout function to your data variable
	(example: wrfout= 'wrfout_d01_2007-10-21_12:00:00';)
    3) Run matlab (you can do it in wrf_fire2/test/em_fire folder and then navigate to the 
	   "run" folder from there)
	4) Run main_function.m   
	5) Output will be in data_out_steps.txt (it contains the description how many points were 
	changed at each step),
	data_out_tign_fstep.txt - level set function after we run the algorythm;
	The data  is written using dlmwrite.

% If you use different from my example perimeter data and wrfout, then
specify their names when you run perimeter.m
  
 The rows of your perimeter file should be:
                    1rt row - time_now (second number is not needed, is set to 0);
                    All next rows - coordinates of all the
                    points on the boundary (lon,lat).






