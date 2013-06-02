Run wrf.exe in ../wrfv2_fire/test/em_fire/hill
namelist.input there is set up with fire_num_ignitions=0
will produce wrfout with fmc_g and fmc_gc set
Run ./fire.exe here to run the fire model from this fmc_g
fire_input.nc is soft link to wrfout in hill
namelist.input has fmoist_run = .false.,  fmoist_interp = .false.
