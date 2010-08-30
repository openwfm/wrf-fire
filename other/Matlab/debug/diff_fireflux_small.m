% test if parallel runs give the same results
runs={'6x1','1x12.a','3x4','4x3'}
runs={'1x1','6x1','1x12.a','1x12','3x4','4x3'}
vars={'UAH','VAH','UF','VF'}
root='fireflux_small-';
wrffile='wrfrst_d01_2006-02-23_12:47:00';
diff_runs(runs,vars,root,wrffile) 
