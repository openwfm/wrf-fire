% How to read surface data into ideal run
%_______________________________________
%
% 1. build ideal.exe for fire : compile em_fire
%
% 2. in namelist.input set the desired land flags for example
%    fire_read_lu=.true.
%    fire_read_fire_ht=.true.
%
% 3. prepare the desired arrays in matlab as 2d at atmosphere or fire size
%    see what is in the arrays by image_array_2d.m
%
% 4. write the arrays into text files for ideal.exe by write_array2d.m
%    with the proper filename (see namelist.input, openwfm.org, Registry/registry.fire 
%    or the source at dyn_em/module_initialize_fire.F for the file names)
%
% 5. you can read the files back by read_array_2d.m
%
