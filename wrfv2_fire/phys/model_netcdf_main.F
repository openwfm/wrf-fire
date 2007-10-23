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

subroutine model_test(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fdx,fdy,wind,alpha,dt)
use module_fr_sfire_util
use module_fr_sfire_model
use module_fr_sfire_phys
use netcdf
use jb_utils
implicit none

!*** arguments
integer, intent(in):: ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme
real, intent(in)::fdx,fdy,dt,wind,alpha

!*** local
integer, parameter:: kw=1                   ! number of wind layers
real, dimension(ifms:ifme,jfms:jfme):: zsf,     &
                 lfn,tign,fuel_frac,                    &
                 grnhft,grnqft
integer:: initialize, ignition,i,j,ifuelread,istep
real:: t0,time_start,sflux,sm,sn
integer, dimension(ifms:ifme,jfms:jfme)::nfuel_cat,ischap
real, dimension(ifms:ifme,jfms:jfme)::fuel_time,vx,vy,dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip
integer::num_tiles
integer, dimension(100)::i_start,i_end,j_start,j_end
character (len=NF90_MAX_NAME), parameter :: iofile = "fire_io.nc"
integer :: ncid,xid,yid,tid,lfnid,vfxid,vfyid,tignid,grnhftid,itmp,tdim,tvalsid
integer :: ostat,stepnum,grnqftid,fuel_fracid,zsfid
real :: tvals
integer,parameter :: ndims = 3
integer,dimension(ndims):: dimids,count,start,atmp

!*** executable

call set_tiles(1,1,ifts,ifte,jfts,jfte,num_tiles,i_start,i_end,j_start,j_end)

!print *,'mesh size in cells: ',ifts,ifte,jfts,jfte
!print *,'array allocation:   ',ifms,ifme,jfms,jfme

t0=0                                        ! starting time, arbitrary

ostat=nf90_open(iofile,NF90_WRITE,ncid)
!print*,ostat,nf90_noerr
if(ostat.ne.NF90_NOERR)then

! populate the arrays somehow
do j=jfts,jfte+1
    do i=ifts,ifte+1
        zsf(i,j)=1000   ! flat ground
        vx(i,j)=wind*cos(alpha)    ! constant wind
        vy(i,j)=wind*sin(alpha)
    enddo
enddo

call check( nf90_create(iofile,NF90_NOCLOBBER,ncid) )
call check( nf90_def_dim(ncid,"x",ifme-ifms+1,xid) )
call check( nf90_def_dim(ncid,"y",jfme-jfms+1,yid) )
call check( nf90_def_dim(ncid,"Time",NF90_UNLIMITED,tid) )
dimids=(/xid,yid,tid/)
call check( nf90_put_att(ncid,NF90_GLOBAL,"DX",fdx) )
call check( nf90_put_att(ncid,NF90_GLOBAL,"DY",fdy) )
stepnum=0
call check( nf90_put_att(ncid,NF90_GLOBAL,"stepnum",stepnum) )
call check( nf90_def_var(ncid,"lfn",NF_TYPE,dimids,lfnid) )
call check( nf90_def_var(ncid,"vx",NF_TYPE,dimids,vfxid) )
call check( nf90_def_var(ncid,"vy",NF_TYPE,dimids,vfyid) )
call check( nf90_def_var(ncid,"tign",NF_TYPE,dimids,tignid) )
call check( nf90_def_var(ncid,"Times",NF_TYPE,(/tid/),tvalsid) )
call check( nf90_def_var(ncid,"grnhft",NF_TYPE,dimids,grnhftid) )
call check( nf90_def_var(ncid,"grnqft",NF_TYPE,dimids,grnqftid) )
call check( nf90_def_var(ncid,"fuelfrac",NF_TYPE,dimids,fuel_fracid) )
call check( nf90_def_var(ncid,"zsf",NF_TYPE,dimids,zsfid) ) 
call check( nf90_enddef(ncid) )
tvals=0.0
tdim=0

else

call check( nf90_inq_dimid(ncid,"x",xid) )
call check( nf90_inquire_dimension(ncid,xid,len=itmp) )
if(itmp.ne.ifme-ifms+1)call check(NF90_EVARSIZE)
call check( nf90_inq_dimid(ncid,"y",yid) )
call check( nf90_inquire_dimension(ncid,yid,len=itmp) )
if(itmp.ne.jfme-jfms+1)call check(NF90_EVARSIZE)
call check( nf90_inq_dimid(ncid,"Time",tid) )
call check( nf90_inquire(ncid,unlimitedDimID=itmp) )
if(itmp.ne.tid)call check(NF90_EVARSIZE)
call check( nf90_inq_varid(ncid,"lfn",lfnid) )
call check( nf90_inq_varid(ncid,"vx",vfxid) )
call check( nf90_inq_varid(ncid,"vy",vfyid) )
call check( nf90_inq_varid(ncid,"tign",tignid) )
call check( nf90_inq_varid(ncid,"grnhft",grnhftid) )
call check( nf90_inq_varid(ncid,"grnqft",grnqftid) )
call check( nf90_inq_varid(ncid,"fuelfrac",fuel_fracid) )
call check( nf90_inq_varid(ncid,"zsf",zsfid) )
call check( nf90_inquire_dimension(ncid,tid,len=tdim) )
call check( nf90_inq_varid(ncid,"Times",tvalsid) )
call check( nf90_get_var(ncid,tvalsid,atmp,start=(/tdim/),count=(/1/)) )
tvals=atmp(1)
dimids=(/xid,yid,tid/)
call check( nf90_get_att(ncid,nf90_global,"stepnum",stepnum) )


count=(/ifme-ifms+1,jfme-jfms+1,1/)
    start=(/1,1,tdim/)

    call check( nf90_get_var(ncid,lfnid,lfn,start=start,count=count) )
    call check( nf90_get_var(ncid,tignid,tign,start=start,count=count) )
    call check( nf90_get_var(ncid,vfxid,vx,start=start,count=count) )
    call check( nf90_get_var(ncid,vfyid,vy,start=start,count=count) )
    call check( nf90_get_var(ncid,grnhftid,grnhft,start=start,count=count) )
    call check( nf90_get_var(ncid,grnqftid,grnqft,start=start,count=count) )
    call check( nf90_get_var(ncid,fuel_fracid,fuel_frac,start=start,count=count) )
    call check( nf90_get_var(ncid,zsfid,zsf,start=start,count=count) )
endif


count=(/ifme-ifms+1,jfme-jfms+1,1/)
! fuel data
ifuelread=1
call set_nfuel_cat( ifts,ifte,jfts,jfte,               &
                    ifms,ifme,jfms,jfme,               &
                    ifuelread,zsf,nfuel_cat)


! for matlab
!open(1,file='model_test_out.txt',form='formatted')
!1   format(e25.12e3)
sm=ifte-ifts+1
sn=jfte-jfts+1
!write(1,1)1.,1.,fdx
!write(1,1)1.,1.,fdy

time_start=t0
stepnum=stepnum+1
istep=stepnum
    if(istep.eq.1)then
            initialize=1
            ignition=0
    elseif(istep.eq.2)then
            initialize=0
            ignition=1
    else
            initialize=0
            ignition=0
    endif
    call sfire_model (initialize, ignition,       &
                 ifts,ifte,jfts,jfte,               &
                 ifms,ifme,jfms,jfme,               &
                 num_tiles,i_start,i_end,j_start,j_end, & ! tiling
                 time_start,dt,                     &
                 fdx,fdy,                           &
                 nfuel_cat,zsf,vx,vy,              &! in arrays
                 lfn,tign,fuel_frac,                & ! state arrays
                 grnhft,grnqft,                      & ! out arrays
                 fuel_time,                         & ! const arrays
                 dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip,ischap &
                 )
    tdim=tdim+1
    start=(/1,1,tdim/)
    atmp(1)=tvals+dt
    call check( &
    nf90_put_var(ncid,tvalsid,atmp,start=(/start(3)/),count=(/count(3)/)) &
               )
    call check( nf90_put_var(ncid,lfnid,lfn,start=start,count=count) )
    call check( nf90_put_var(ncid,tignid,tign,start=start,count=count) )
    call check( nf90_put_var(ncid,vfxid,vx,start=start,count=count) )
    call check( nf90_put_var(ncid,vfyid,vy,start=start,count=count) )
    call check( nf90_put_var(ncid,grnhftid,grnhft,start=start,count=count) )
    call check( nf90_put_var(ncid,grnqftid,grnqft,start=start,count=count) )
    call check( nf90_put_var(ncid,fuel_fracid,fuel_frac,start=start,count=count) )
    call check( nf90_put_var(ncid,zsfid,zsf,start=start,count=count) )
    call check( nf90_put_var(ncid,tvalsid,atmp,start=(/tdim/),count=(/1/)) )
    call check( nf90_put_att(ncid,nf90_global,"stepnum",istep) )
    call check( nf90_sync(ncid) )
    sflux=avg_2darray      ( ifts,ifte,jfts,jfte,               &
                             ifms,ifme,jfms,jfme,               &
                             grnhft)
!    print *,'step ',istep,' of ',msteps,' time ',time_start,' avg flux ',sflux
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

nx=200
ny=200
fdx=50
fdy=50
dt=5
wind=5
alpha=1.3

!print *,'nx=',nx,' ny=',ny,' msteps=',msteps,' fdx=',fdx,' fdy=',fdy,' dt=',dt
call model_test(1,nx,1,ny,1,nx+1,1,ny+1,fdx,fdy,wind,alpha,dt)

end program model_test_main


