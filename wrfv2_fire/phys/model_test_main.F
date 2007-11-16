! the main program is at the end because of fortran limitations

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
real:: t0,time_start,sm,sn
integer, dimension(ifms:ifme,jfms:jfme)::nfuel_cat,ischap
real, dimension(ifms:ifme,jfms:jfme)::fuel_time,vx,vy,dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip
integer::num_tiles
integer, dimension(100)::i_start,i_end,j_start,j_end

!*** executable

call set_tiles(1,1,ifps,ifpe,jfps,jfpe,num_tiles,i_start,i_end,j_start,j_end)

print *,'mesh size in cells: ',ifps,ifpe,jfps,jfpe
print *,'array allocation:   ',ifms,ifme,jfms,jfme

t0=0                                        ! starting time, arbitrary

! populate the arrays somehow
do j=jfps,jfpe+1
    do i=ifps,ifpe+1
        zsf(i,j)=1000   ! flat ground
        vx(i,j)=wind*cos(alpha)    ! constant wind
        vy(i,j)=wind*sin(alpha)
    enddo
enddo

! fuel data
ifuelread=0
nfuel_cat0=3

! for matlab
open(1,file='model_test_out.txt',form='formatted')
1   format(e25.12e3)
sm=ifpe-ifps+1
sn=jfpe-jfps+1
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
    nfuel_cat,                              & ! fuel data per point 
    fuel_time,                              & ! save derived internal data
    dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip,ischap &
    ) 
    if(mod(istep,50).eq.0)then
        write(1,1)1.,1.,time_start
        write(1,1)sm+1,sn+1,((lfn(i,j),i=ifps,ifpe+1),j=jfps,jfpe+1)
        write(1,1)sm+1,sn+1,((tign(i,j),i=ifps,ifpe+1),j=jfps,jfpe+1)
        write(1,1)sm+1,sn+1,((vx(i,j),i=ifps,ifpe+1),j=jfps,jfpe+1)
        write(1,1)sm+1,sn+1,((vy(i,j),i=ifps,ifpe+1),j=jfps,jfpe+1)
        write(1,1)sm,sn,((grnhfx(i,j),i=ifps,ifpe),j=jfps,jfpe) 
    endif
    print *,'test_main: step ',istep,' of ',msteps,' time ',time_start
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
real:: fdx,fdy, dt,wind,alpha       ! fire mesh spacing (m), time step (s), wind azimuth
integer::rfx,rfy ! refinement, convenience only

nx=400
ny=400
msteps=200
fdx=6
fdy=6

rfx=1
rfy=1

nx=nx*rfx
ny=ny*rfy
fdx=fdx/rfx
fdy=fdy/rfy

dt=1
wind=10
alpha=0.0

print *,'nx=',nx,' ny=',ny,' msteps=',msteps,' fdx=',fdx,' fdy=',fdy,' dt=',dt,' wind=',wind,' alpha=',alpha
call model_test(1,nx,1,ny, &
   1,nx+1,1,ny+1, &
   1,nx,1,ny, &
   fdx,fdy,wind,alpha,dt,msteps)

end program model_test_main
