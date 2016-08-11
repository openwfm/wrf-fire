%make sure this is in each sub-folder of /wrf-fire/wrfv2_fire/test/

f='wrfout_d01_2013-08-13_00:00:00'; 
w=read_wrfout_tign(f);
save w.mat w 