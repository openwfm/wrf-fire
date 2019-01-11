IMPORTANT: To initialize Matlab environment for wrf-sfire, do in Matlab first:
cd ..; startup; cd ROS

Files:

ros_balbi.m                Matlab function to compute rate of spread from fuel properties, 
                           wind, slope, and fuel moisture, by Balbi model.

ros_rothermel.m            Matlab function to compute rate of spread from fuel properties, 
                           wind, slope, and fuel moisture, by Rothermel model. 

fuels.m                    Script written by iwrf-sfire (wrf.exe) every time the fire model is started. 
                           Running fuels in Matlab creates structure array named fuel. Use fuel(n) 
                           as input to the other routines for fuel model number n. The structure 
                           fuel(n) contains decription of fuel parameters that the run of wrf-sfire used,
                           and rate of spread (ROS) evaluated in the Fortran wrf-sfire code at a number 
                           slope, wind speed, and fuel moisture values. Parameter fuel(n).ibeh=1 
                           means that Rothermel model was selected, and fuel(n).ibeh=2 Balbi model.

check_ros.m                Run the spread model in Matlab and compare with the values computed in wrf-sfire 
                           and stored in the fuel structure, for all fuel models. If all is good, it
                           should return number less than 1e-4.
 
test_ros_balbi.m           A simple driver to test ros_balbi.m

fire_ros_balbi_moodymjm.m  Another version of Balbi model by Matt Moody.

plot_fuel.m (in ../vis3d)  Generate pictures of rate of spread as function of slope, wind, and fuel moisture,
                           using the values computed in wrf-sfire and stored in the fuel structure, and the values
                           computed in Matlab. These should be the same, so if all is well only one curve is visible. 
               




