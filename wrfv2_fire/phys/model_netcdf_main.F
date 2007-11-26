! the main program is at the end because of fortran limitations
#ifndef NF_TYPE
#define NF_TYPE NF90_FLOAT
#endif

module jb_utils
contains

subroutine check(status)
use netcdf
  integer, intent ( in) :: status
  
  if(status /= nf90_noerr) then 
    print *, nf90_strerror(status)
    stop "Stopped"
  end if
end subroutine check 

end module jb_utils

module module_model_test
contains

!
!******************************
!

subroutine model_test(  &
    ifds,ifde,jfds,jfde, &
    ifms,ifme,jfms,jfme, &
    ifps,ifpe,jfps,jfpe, &
    fdx,fdy,wind,alpha,dt,msteps)
use module_fr_sfire_util
use module_fr_sfire_model
use module_fr_sfire_phys
use netcdf
use jb_utils
implicit none

!*** arguments
integer, intent(in):: &
    ifds,ifde,jfds,jfde, &
    ifps,ifpe,jfps,jfpe, &
    ifms,ifme,jfms,jfme, msteps
real, intent(in)::fdx,fdy,dt,wind,alpha

!*** local
real, dimension(ifms:ifme,jfms:jfme):: zsf,     &
                 lfn,tign,fuel_frac,                    &
                 grnhfx,grnqfx
integer:: initialize, ignition,i,j,ifuelread,istep,nfuel_cat0
real:: t0,time_start,shflx,sqflx,sm,sn
integer, dimension(ifms:ifme,jfms:jfme)::nfuel_cat,ischap
real, dimension(ifms:ifme,jfms:jfme)::fuel_time,vx,vy,dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip
integer::num_tiles
integer, dimension(100)::i_start,i_end,j_start,j_end
character (len=NF90_MAX_NAME), parameter :: iofile = "fire_io.nc"
integer :: ncid,xid,yid,tid,lfnid,vfxid,vfyid,tignid,grnhftid,itmp,tdim,tvalsid
integer :: xsid,ysid  ! staggered dims
integer :: ostat,stepnum,grnqftid,fuel_fracid,zsfid,fueltimeid
real :: tvals
integer,parameter :: ndims = 3
integer,dimension(ndims):: dimids,count,start,atmp,counts,starts

!*** executable

call set_tiles(1,1,ifps,ifpe,jfps,jfpe,num_tiles,i_start,i_end,j_start,j_end)


t0=0                                        ! starting time, arbitrary
ostat=nf90_open(iofile,NF90_WRITE,ncid)
!print*,ostat,nf90_noerr
if(ostat.ne.NF90_NOERR)then

! populate the arrays somehow
do j=jfps,jfpe+1
    do i=ifps,ifpe+1
        zsf(i,j)=1000   ! flat ground
        vx(i,j)=wind*cos(alpha)    ! constant wind
        vy(i,j)=wind*sin(alpha)
    enddo
enddo

call check( nf90_create(iofile,NF90_NOCLOBBER,ncid) )
call check( nf90_def_dim(ncid,"x",ifpe-ifps+1,xid) )
call check( nf90_def_dim(ncid,"y",jfpe-jfps+1,yid) )
call check( nf90_def_dim(ncid,"x_stag",ifpe-ifps+2,xsid) )
call check( nf90_def_dim(ncid,"y_stag",jfpe-jfps+2,ysid) )
call check( nf90_def_dim(ncid,"Time",NF90_UNLIMITED,tid) )
call check( nf90_put_att(ncid,NF90_GLOBAL,"DX",fdx) )
call check( nf90_put_att(ncid,NF90_GLOBAL,"DY",fdy) )
stepnum=0
call check( nf90_put_att(ncid,NF90_GLOBAL,"stepnum",stepnum) )

dimids=(/xsid,ysid,tid/)
call check( nf90_def_var(ncid,"lfn",NF_TYPE,dimids,lfnid) )
call check( nf90_def_var(ncid,"vx",NF_TYPE,dimids,vfxid) )
call check( nf90_def_var(ncid,"vy",NF_TYPE,dimids,vfyid) )
call check( nf90_def_var(ncid,"Times",NF_TYPE,(/tid/),tvalsid) )

dimids=(/xid,yid,tid/)
call check( nf90_def_var(ncid,"tign",NF_TYPE,dimids,tignid) )
call check( nf90_def_var(ncid,"grnhfx",NF_TYPE,dimids,grnhftid) )
call check( nf90_def_var(ncid,"grnqfx",NF_TYPE,dimids,grnqftid) )
call check( nf90_def_var(ncid,"fuelfrac",NF_TYPE,dimids,fuel_fracid) )
call check( nf90_def_var(ncid,"fueltime",NF_TYPE,dimids,fueltimeid) )
call check( nf90_enddef(ncid) )
tvals=0.0
tdim=0

else

call check( nf90_inq_dimid(ncid,"x_stag",xsid) )
call check( nf90_inquire_dimension(ncid,xsid,len=itmp) )
if(itmp.ne.ifpe-ifps+2)call check(NF90_EVARSIZE)
call check( nf90_inq_dimid(ncid,"y_stag",ysid) )
call check( nf90_inquire_dimension(ncid,ysid,len=itmp) )
if(itmp.ne.jfpe-jfps+2)call check(NF90_EVARSIZE)

call check( nf90_inq_dimid(ncid,"x",xid) )
call check( nf90_inquire_dimension(ncid,xid,len=itmp) )
if(itmp.ne.ifpe-ifps+1)call check(NF90_EVARSIZE)
call check( nf90_inq_dimid(ncid,"y",yid) )
call check( nf90_inquire_dimension(ncid,yid,len=itmp) )
if(itmp.ne.jfpe-jfps+1)call check(NF90_EVARSIZE)
call check( nf90_inq_dimid(ncid,"Time",tid) )
call check( nf90_inquire(ncid,unlimitedDimID=itmp) )
if(itmp.ne.tid)call check(NF90_EVARSIZE)

call check( nf90_inq_varid(ncid,"lfn",lfnid) )
call check( nf90_inq_varid(ncid,"vx",vfxid) )
call check( nf90_inq_varid(ncid,"vy",vfyid) )
call check( nf90_inq_varid(ncid,"tign",tignid) )
call check( nf90_inq_varid(ncid,"grnhfx",grnhftid) )
call check( nf90_inq_varid(ncid,"grnqfx",grnqftid) )
call check( nf90_inq_varid(ncid,"fuelfrac",fuel_fracid) )
call check( nf90_inq_varid(ncid,"fueltime",fueltimeid) )
call check( nf90_inquire_dimension(ncid,tid,len=tdim) )
call check( nf90_inq_varid(ncid,"Times",tvalsid) )
call check( nf90_get_var(ncid,tvalsid,atmp,start=(/tdim/),count=(/1/)) )
tvals=atmp(1)
dimids=(/xid,yid,tid/)
call check( nf90_get_att(ncid,nf90_global,"stepnum",stepnum) )

count=(/ifpe-ifps+1,jfpe-jfps+1,1/)
counts=(/ifpe-ifps+2,jfpe-jfps+2,1/)
start=(/1,1,tdim/)
starts=(/1,1,tdim/)

#define _SD (ifps:ifpe+1,jfps:jfpe+1)
#define _D  (ifps:ifpe,jfps:jfpe)
call check( nf90_get_var(ncid,lfnid,lfn _SD,start=starts,count=counts) )
call check( nf90_get_var(ncid,tignid,tign _D,start=starts,count=count) )
call check( nf90_get_var(ncid,vfxid,vx _SD,start=starts,count=counts) )
call check( nf90_get_var(ncid,vfyid,vy _SD,start=starts,count=counts) )
call check( nf90_get_var(ncid,grnhftid,grnhfx _D,start=start,count=count) )
call check( nf90_get_var(ncid,grnqftid,grnqfx _D,start=start,count=count) )
call check( nf90_get_var(ncid,fuel_fracid,fuel_frac _D,start=start,count=count) )
call check( nf90_get_var(ncid,fueltimeid,fuel_time _D,start=start,count=count) )
endif


count=(/ifpe-ifps+1,jfpe-jfps+1,1/)
counts=(/ifpe-ifps+2,jfpe-jfps+2,1/)
! fuel data
ifuelread=0
nfuel_cat0=3
!call set_nfuel_cat( ifps,ifpe,jfps,jfpe,               &
!                    ifms,ifme,jfms,jfme,               &
!                    ifuelread,zsf,nfuel_cat)


sm=ifpe-ifps+1
sn=jfpe-jfps+1
t0=tvals
time_start=t0
stepnum=stepnum+1
istep=stepnum
if(istep.eq.1)then
        initialize=1
        ignition=0
elseif(istep.eq.2)then
        initialize=0
        ignition=2
else
        initialize=0
        ignition=0
endif

call   sfire_model (                    &
initialize, ignition,                   & ! switches
ifuelread,nfuel_cat0,                   &
ifds,ifde,jfds,jfde,                    & ! fire domain dims - the whole domain
ifms,ifme,jfms,jfme,                    & ! fire memory dims - how declared
ifps,ifpe,jfps,jfpe,                    & ! fire patch dims  - this processor
num_tiles,i_start,i_end,j_start,j_end,  & ! fire grid tiling - divide for openmp
time_start,dt,                          & ! time and increment
fdx,fdy,                                & ! fire mesh spacing
zsf,                                    & ! terrain height (for gradient)
vx,vy,                                  & ! input: wind
lfn,tign,fuel_frac,                     & ! state: level function, ign time, fuel left
grnhfx,grnqfx,                          & ! output: heat fluxes
nfuel_cat,                              &
fuel_time,                              & ! save derived internal data
dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip,ischap &
) 
tdim=tdim+1
start=(/1,1,tdim/)
atmp(1)=tvals+dt
call check( &
nf90_put_var(ncid,tvalsid,atmp,start=(/start(3)/),count=(/count(3)/)) &
           )
call check( nf90_put_var(ncid,lfnid,lfn _SD,start=start,count=counts) )
call check( nf90_put_var(ncid,tignid,tign _D,start=start,count=count) )
call check( nf90_put_var(ncid,vfxid,vx _SD,start=start,count=counts) )
call check( nf90_put_var(ncid,vfyid,vy _SD,start=start,count=counts) )
call check( nf90_put_var(ncid,grnhftid,grnhfx _D,start=start,count=count) )
call check( nf90_put_var(ncid,grnqftid,grnqfx _D,start=start,count=count) )
call check( nf90_put_var(ncid,fuel_fracid,fuel_frac _D,start=start,count=count) )
call check( nf90_put_var(ncid,tvalsid,atmp,start=(/tdim/),count=(/1/)) )
call check( nf90_put_var(ncid,fueltimeid,fuel_time _D,start=start,count=count) )
call check( nf90_put_att(ncid,nf90_global,"stepnum",istep) )
call check( nf90_sync(ncid) )
time_start=time_start+dt 
    
!close(1)
call check( nf90_close(ncid) )

end subroutine model_test

end module module_model_test

!
!******************************
!

program model_test_main
use module_model_test
implicit none

!*** declarations

integer:: nx,ny, msteps  ! problem dimension, in cells, number of steps
real:: fdx,fdy, dt,wind,alpha       ! fire mesh spacing (m), time step (s)
integer::rfx,rfy ! refinement, convenience only

nx=200
ny=200
msteps=1
fdx=50
fdy=50

rfx=1
rfy=1

nx=nx*rfx
ny=ny*rfy
fdx=fdx/rfx
fdy=fdy/rfy

dt=1000
wind=5
alpha=1.3

!print *,'nx=',nx,' ny=',ny,' msteps=',msteps,' fdx=',fdx,' fdy=',fdy,' dt=',dt
!call model_test(1,nx,1,ny,1,nx+1,1,ny+1,fdx,fdy,wind,alpha,dt)
call model_test(1,nx,1,ny, &
   1,nx+1,1,ny+1, &
   1,nx,1,ny, &
   fdx,fdy,wind,alpha,dt,msteps)
end program model_test_main


