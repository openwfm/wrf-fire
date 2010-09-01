function diff_fireflux_small(timestep)
% test if parallel runs give the same results
% runs={'12x1','6x1','1x12','3x4','3x4a','4x3'}
% runs={'1x1','6x1','1x12.a','1x12','3x4','3x4a','4x3'}
runs={'1x1a','1x12a','3x4a','12x1','4x3','1x12'}
vars={'UAH','VAH','UF','VF'}
root='fireflux_small-';
wrffile='wrfrst_d01_2006-02-23_12:47:00';
wrffile='wrfout_d01_2006-02-23_12:42:00';
% timestep=10
diff_runs(runs,vars,root,wrffile,timestep) ;
end
