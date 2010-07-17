module wrf_namelist
use wrf_netcdf , only : compat_fire_grid
use module_fr_sfire_util , only : crash
implicit none

character(len=*),parameter::nmlfile='namelist.input'
integer,parameter::max_domains=10,max_moves=1,max_eta=501,max_bogus=5
#include "namelist_defines.inc"
#include "namelist_statements.inc"

namelist /standalone/ compat_fire_grid

contains

subroutine read_namelist()
implicit none
character(len=32)::grp
integer::iou

#include "namelist_defaults.inc"

open(iou,file=nmlfile,err=999)

grp='time_control'
read(iou,time_control,err=998)
rewind(iou)

grp='domains'
read(iou,domains,err=998)
rewind(iou)

grp='fire'
read(iou,fire,err=998)
rewind(iou)

grp='standalone'
read(iou,standalone,err=997,end=997)
close(iou)

997 continue
return
998 continue
call crash('error reading namelist group '//grp)

999 continue
call crash('error opening namelist '//trim(nmlfile))

end subroutine read_namelist

end module wrf_namelist
