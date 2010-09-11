% test if parallel runs give the same results
runs={'1x1','1x6','6x1','2x3','2x3','1x2','2x1'}
vars={'UAH','VAH','UF','VF'}
root='hill-';
wrffile='wrfrst_d01_0001-01-01_00:01:00';
diff_runs(runs,vars,root,wrffile) 
