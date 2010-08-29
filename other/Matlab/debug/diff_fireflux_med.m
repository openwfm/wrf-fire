% test if parallel runs give the same results
runs={'3x12','2x12','36x1'}
vars={'UAH','VAH','UF','VF'}
root='fireflux_med-';
wrffile='wrfrst_d01_2006-02-23_12:43:30';
diff_runs(runs,vars,root,wrffile) 
