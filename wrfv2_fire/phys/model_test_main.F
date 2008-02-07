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
                 grnhfx,grnqfx,lfn_out
integer:: initialize, num_ignitions,i,j,ifuelread,istep,nfuel_cat0,ifun
real:: t0,time_start,sm,sn
integer, dimension(ifms:ifme,jfms:jfme)::nfuel_cat,ischap
real, dimension(ifms:ifme,jfms:jfme)::fuel_time,vx,vy,dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip
integer, parameter::max_tiles=10
integer::num_tiles,ij,ifts,ifte,jfts,jfte
integer, dimension(max_tiles)::if_start,if_end,jf_start,jf_end
integer, parameter :: max_ignitions=10
real, dimension(max_ignitions) :: ignition_start_x,ignition_start_y, &
    ignition_end_x,ignition_end_y,ignition_radius,ignition_time

!*** executable

call set_tiles(1,1,ifps,ifpe,jfps,jfpe,num_tiles,if_start,if_end,jf_start,jf_end)

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
num_ignitions=2
ignition_start_x(1)=0.5*fdx*(ifde-ifds)
ignition_start_y(1)=0.5*fdy*(jfde-jfds)
ignition_end_x(1)=0.5*fdx*(ifde-ifds)*0.9999999
ignition_end_y(1)=0.5*fdy*(jfde-jfds)*1.0000001
ignition_radius(1) = 0.5*max(5.0,6*max(fdx,fdy))
ignition_time(1)=1
ignition_start_x(2)=1000
ignition_start_y(2)=500
ignition_end_x(2)=1500
ignition_end_y(2)=1500
! at least 6 by 6 cells but no less than 5 m
ignition_radius(2) = 0.5*max(5.0,6*max(fdx,fdy))
ignition_time(2)=2


do istep=1,msteps
    if(istep.eq.1)then
            initialize=1
    else
            initialize=0
    endif
    do ifun=1,3
        ! 1 = initialize (winds come here too)
        ! 2 = timestep, lfn -> lfn_out, update tign
        ! 3 = fuel and fluxes

        if(ifun.eq.3)then
            ! halo tign lfn width 1 (just to get the nodes of the last row of cells)
        endif
        if(ifun.eq.2)then
            ! halo lfn width 2 
        endif

!OMP    PARALLEL DO PRIVATE(ij,ifts,ifte,jfts,jfte)        
        do ij=1,num_tiles
            ifts= if_start(ij)          
            ifte= if_end(ij)
            jfts= jf_start(ij)
            jfte= jf_end(ij)

            call   sfire_model (istep,ifun,         &
                initialize, num_ignitions,              & 
                ifuelread,nfuel_cat0,                   &
                ifds,ifde,jfds,jfde,                    & ! fire domain dims - the whole domain
                ifms,ifme,jfms,jfme,                    & ! fire memory dims - how declared
                ifts,ifte,jfts,jfte,                    & ! fire patch dims  - this processor
                time_start,dt,                          & ! time and increment
                fdx,fdy,                                & ! fire mesh spacing
                ignition_start_x,ignition_start_y,      &
                ignition_end_x,ignition_end_y,          &  
                ignition_radius,                        &
                ignition_time,                          &
                zsf,                                    & ! terrain height (for gradient)
                vx,vy,                                  & ! input: wind
                lfn,lfn_out,tign,fuel_frac,                     & ! state: level function, ign time, fuel left
                grnhfx,grnqfx,                          & ! output: heat fluxes
                nfuel_cat,                              & ! fuel data per point 
                fuel_time,                              & ! save derived internal data
                dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip,ischap &
            )
        enddo 
        
        if(ifun.eq.2)then 
            ! all done, can copy lfn_out to lfn without racing someone else's input from lfn
!OMP        PARALLEL DO PRIVATE(ij,ifts,ifte,jfts,jfte,i,j)        
            do ij=1,num_tiles
                ifts= if_start(ij)          
                ifte= if_end(ij)
                jfts= jf_start(ij)
                jfte= jf_end(ij)
                do j=jfts,enode(jfte,jfde)
                    do i=ifts,enode(ifte,ifde)
                        lfn(i,j)=lfn_out(i,j) ! at this point lfn_out is synchronized, can copy
                    enddo
                enddo
            enddo
        endif
    enddo
    if(istep.le.10.or.mod(istep,50).eq.0)then
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
msteps=6
!msteps=3
fdx=6
fdy=6

rfx=1
rfy=1

nx=nx*rfx
ny=ny*rfy
fdx=fdx/rfx
fdy=fdy/rfy

dt=0.5
wind=10
alpha=0.0

print *,'nx=',nx,' ny=',ny,' msteps=',msteps,' fdx=',fdx,' fdy=',fdy,' dt=',dt,' wind=',wind,' alpha=',alpha
call model_test(1,nx,1,ny, &
   -1,nx+3,0,ny+2, &
   1,nx,1,ny, &
   fdx,fdy,wind,alpha,dt,msteps)

end program model_test_main
