file='wrfrst_d01_0001-01-01_00:01:00'
case1='wind2'
case2='master'
root='/storage/jmandel'
fmt='%s/wrf-fire.%s/wrfv2_fire/test/em_fire/hill/%s'
nc1=sprintf(fmt,root,case1,file)
nc2=sprintf(fmt,root,case2,file)
ncdiff(nc1,nc2)
