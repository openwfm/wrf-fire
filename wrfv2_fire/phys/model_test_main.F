program model_test
use module_fr_sfire_model
implicit none

!*** declarations
integer, parameter:: nx=100,ny=130          ! problem dimension, in cells
integer, parameter:: ifts=1,ifte=nx,jfts=1,jfte=ny
integer, parameter:: ifms=1,ifme=nx+1,jfms=1,jfme=ny+1
integer, parameter:: kw=1                   ! number of wind layers
real, parameter:: fdx=50.,fdy=50.           ! fire mesh spacing (m)
real, dimension(ifms:ifme,jfms:jfme):: zsf,     &
                 lfn,tign,fuel_frac,                    &
                 grnhft,grnqft
real, dimension(ifms:ifme,jfms:jfme,kw):: vfx,vfy
integer:: initialize, ignition,i,j,ifuelread,istep
real:: t0,dt,time_start
integer, dimension(ifms:ifme,jfms:jfme)::nfuel_cat

!*** executable
t0=1                                        ! starting time, arbitrary
dt=10                                       ! the fire time step (can be large)

! populate the arrays somehow
do j=jfts,jfte+1
    do i=ifts,ifte+1
        zsf(i,j)=1000   ! flat ground
        vfx(i,j,1)=5    ! constant wind
        vfy(i,j,1)=-3
    enddo
enddo

! fuel data
ifuelread=1
call set_nfuel_cat( ifts,ifte,jfts,jfte,               &
                    ifms,ifme,jfms,jfme,               &
                    ifuelread,zsf,nfuel_cat)

open(1,file='model_test_out.txt')
write(1,'(i6)')nx,ny
do istep=0,100
    time_start=t0+istep*dt
    if(i.eq.1)then
        initialize=1
        ignition=1
    else
        initialize=0
        ignition=0
    endif
    call sfire_model (initialize, ignition,       &
                 ifts,ifte,jfts,jfte,               &
                 ifms,ifme,jfms,jfme,               &
                 kw,                                &
                 time_start,dt,                     &
                 fdx,fdy,                           &
                 nfuel_cat,zsf,vfx,vfy,             &
                 lfn,tign,fuel_frac,                &
                 grnhft,grnqft)
    write(1,1)((lfn(i,j),i=ifts,ifte+1),j=jfts,jfte+1)
    write(1,1)((tign(i,j),i=ifts,ifte+1),j=jfts,jfte+1)
    write(1,1)((grnhft(i,j),i=ifts,ifte),j=jfts,jfte) 
1   format(e25.15)
enddo


end program model_test
