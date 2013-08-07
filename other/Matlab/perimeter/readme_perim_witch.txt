1) Finish this and submit;
2) Write Jan that it is related to his commit;
3) Write the comments for the perimeter_in code;



Volodymyr Kondratenko 8/7/2013
% This is the code that computes the level set function based on its given perimeter and 
  time of ignitions of the points on the perimeter

1) Diretions about how to run the code;
2) Description of the input;
3) Description of the output;

(1) Please see the directions about how to run the code:
   
   A) If you are running the small/ideal case and there is no need to run
      a job, then you:
	  - modify main_function.m (see how in (2))
	  - ln -s your wrfout file to the "perimeter" folder;
	  - run matlab and then run main_function.m
   
   B) If you are running a real case example and need to submit a job then:

      1)  ln -s your wrfout file to the "perimeter" folder;
      2)  You create a run2.m file in the folder wrf-fire/wrfv2_fire/test/em_fire,
	  that contains:
	  cd ../../../other/Matlab/perimeter/
	  main_function

	  3) You go back to the perimeter folder and modify main_function.m for your case;
	  4) qsub test_run.pbs;

(2) Description of the input:
    
	All the input variables lie in the main_funtion.m and you need to modify this
	file according to your real case;
    Description of the input variables:
	1) data - empty if you are using tign_g from the wrfout to determine the
	   perimeter of the file, otherwise it contains time_now and coordinates 
	   of the perimeter of the fire;
    2) wrfout='name of the wrfout file taht you are using', do not forget to 
	   ln -s the wrfout file to the perimeter folder;
    3) interval- the length of each timestep in wrf-fire is 15min=900sec;
	   count- how many timesteps you jump over each iteration;
	   Example: each time_step in wrfout is 15minutes, thus interval=15*60=900sec;
	   You want to update every hour, so 60min=4*15, and so count=4;
	4) time - the position of the time_now in the "Times" array in the wrfout file;
	5) time_now - time of the fire perimeter;

(3) Description of the output:
    Output will be printed to the output_tign.txt, that contains the tign of the 
	area inside the fire_perimeter;
	You can read it in matlab using dlmread:
	tign=dlmread('output_tign.txt');

