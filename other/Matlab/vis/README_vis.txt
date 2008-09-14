Matlab routines to visualized coupled model from files written by sfire

1. in namelist.input set fire_print_file=1 (in test/em_fire or test/em_real)
2. in phys/module_fr_sfire_core.F and model.F uncomment #define DEBUG_OUT
3. build and run wrf *on a single processor*
4. start matlab in test/em_fire or test_em_real
5. >> mpath; go
6. which vc2d, edit upper bound to the number of timesteps desired

works with commit f6e1299c976cc23224fd889ecf10fc1d7ac17855 Sep 14 2008

jm
