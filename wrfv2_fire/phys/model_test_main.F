! the main program is at the end because of fortran limitations

module module_model_test
contains

!
!******************************
!

subroutine model_test(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fdx,fdy,wind,alpha,dt,msteps)
use module_fr_sfire_util
use module_fr_sfire_model
use module_fr_sfire_phys
implicit none

!*** arguments
integer, intent(in):: ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme, msteps
real, intent(in)::fdx,fdy,dt,wind,alpha

!*** local
integer, parameter:: kw=1                   ! number of wind layers
real, dimension(ifms:ifme,jfms:jfme):: zsf,     &
                 lfn,tign,fuel_frac,                    &
                 grnhft,grnqft
real, dimension(ifms:ifme,jfms:jfme,kw):: vfx,vfy
integer:: initialize, ignition,i,j,ifuelread,istep
real:: t0,time_start,sflux,sm,sn
integer, dimension(ifms:ifme,jfms:jfme)::nfuel_cat,ischap
real, dimension(ifms:ifme,jfms:jfme)::fuel_time,vx,vy,dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip
integer::xifms,xifme,xjfms,xjfme
integer::num_tiles
integer, dimension(100)::i_start,i_end,j_start,j_end

!*** executable
xifms=ifms
xifme=ifme
xjfms=jfms
xjfme=jfme

call set_tiles(1,1,ifts,ifte,jfts,jfte,num_tiles,i_start,i_end,j_start,j_end)

print *,'mesh size in cells: ',ifts,ifte,jfts,jfte
print *,'array allocation:   ',ifms,ifme,jfms,jfme

t0=0                                        ! starting time, arbitrary

! populate the arrays somehow
do j=jfts,jfte+1
    do i=ifts,ifte+1
        zsf(i,j)=1000   ! flat ground
        vfx(i,j,1)=wind*cos(alpha)    ! constant wind
        vfy(i,j,1)=wind*sin(alpha)
    enddo
enddo

! fuel data
ifuelread=1
call set_nfuel_cat( ifts,ifte,jfts,jfte,               &
                    ifms,ifme,jfms,jfme,               &
                    ifuelread,zsf,nfuel_cat)


! for matlab
open(1,file='model_test_out.txt',form='formatted')
1   format(e25.12e3)
sm=ifte-ifts+1
sn=jfte-jfts+1
write(1,1)1.,1.,fdx
write(1,1)1.,1.,fdy

time_start=t0
do istep=1,msteps
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
                 kw,                                &
                 time_start,dt,                     &
                 fdx,fdy,                           &
                 nfuel_cat,fuel_time,zsf,vfx,vfy,   &
                 lfn,tign,fuel_frac,                &
                 grnhft,grnqft                      &
#                include "fr_sfire_params_args.inc" 
                 )
    write(1,1)1.,1.,time_start
    write(1,1)sm+1,sn+1,((lfn(i,j),i=ifts,ifte+1),j=jfts,jfte+1)
    write(1,1)sm+1,sn+1,((tign(i,j),i=ifts,ifte+1),j=jfts,jfte+1)
    write(1,1)sm+1,sn+1,((vfx(i,j,1),i=ifts,ifte+1),j=jfts,jfte+1)
    write(1,1)sm+1,sn+1,((vfy(i,j,1),i=ifts,ifte+1),j=jfts,jfte+1)
    write(1,1)sm,sn,((grnhft(i,j),i=ifts,ifte),j=jfts,jfte) 
    sflux=avg_2darray      ( ifts,ifte,jfts,jfte,               &
                             ifms,ifme,jfms,jfme,               &
                             grnhft)
    print *,'step ',istep,' of ',msteps,' time ',time_start,' avg flux ',sflux
    time_start=time_start+dt 
enddo

close(1)

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
msteps=200
fdx=50
fdy=50
dt=10
wind=5
alpha=1.3

print *,'nx=',nx,' ny=',ny,' msteps=',msteps,' fdx=',fdx,' fdy=',fdy,' dt=',dt
call model_test(1,nx,1,ny,1,nx+1,1,ny+1,fdx,fdy,wind,alpha,dt,msteps)

end program model_test_main
