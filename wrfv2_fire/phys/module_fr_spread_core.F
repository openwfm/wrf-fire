module module_fr_spread_core
!
!*** Jan Mandel August 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
contains

subroutine fire_spread_core( ids,ide,jds,jde,ims,ime,jms,jme,&
time_start,time_end,fire_dx,fire_dy,fuel_time,               & 
vx,vy,r,                                                     &
phi,tign,frac_lost,frac_end) 

!*** purpose
!
! Dynamical core of the fire spread model, insulated from the physics. 
! References the fire grid ONLY. Keep separated from the coupling
! with atm. model and data input.
!

use module_fr_fuelused
use module_fr_propagate

implicit none

!*** arguments

integer, intent(in) :: ids,ide,jds,jde,ims,ime,jms,jme
real, intent(inout), dimension(ims:ime,jms:jme)::phi,tign ! state
real, intent(in), dimension(ims:ime,jms:jme)::vx,vy,r  
real, intent(in), dimension(ims:ime-1,jms:jme-1)::fuel_time
real, intent(out), dimension(ims:ime-1,jms:jme-1)::frac_lost,frac_end
real, intent(in):: time_start,time_end,fire_dx,fire_dy

! argument          intent  description                     (unit)  lives at
!
! ids,ide,jds,jde   in   mesh domain dimensions              (1)
! ims,ime,jms,jme   in   mesh aray dimensions                (1)
! time_start        in   the starting time                   (s)
! time_end          in   the ending time                     (s)
! fire_dx,fire_dy   in   fire mesh spacings                  (m)
! fuel_time         in   time fuel burns down to 1/e         (s)      cells
! vx,vy             in   the wind velocity                   (m/s)    nodes 
! r                 in   the spread rate w/o wind            (m/s)    nodes
! phi             inout  level function (state)              (1)      nodes
! tign            inout  ignition time (state)               (s)      nodes
! frac_lost         out  the fuel fraction decrease          (1)      cells 
! frac_end          out  the fuel fraction at the end        (1)      cells

! ***NOTE: vx,vy,r will be replaced by a call to a speed function in future.***
! The speed function will be called for the spread rate at points of fireline. 
! A version of such level set code is in progress.

!*** description

! This is a dynamical core of the fire spread model, insulated from the physics. 
! The physics should be done in the pre- and postprocessing, and in 
! the speed function for the fireline propagation (future).
!
! The state of the model is the level function phi, which determines the fire 
! area, and the ignition time tign, both interpolated from values at nodes.
! The fire area is the level set where phi <= 0.  The fireline is where phi=0. 
! The array tign outside of the fire area is not set or referenced. 
! The state should be preserved between the calls, and it can be modified by
! data assimilation. All other quantities are derived from the state in each call.
!
! The level function evolves the fireline with the speed in the normal direction given by
! the spread rate r and the normal component of the wind. The level set method
! takes care of of various special cases automagically, such as ignition of a cell 
! surrounded by cells that all completely burning, and merging of approaching firelines.
!
! The fuel fraction is estimated from the the ignition times assuming 
! exponential decrease since ignition with decrease of fule fraction to 1/e in
! fuel_tim. The ignition times at nodes are interpolated linearly from the 
! evolving level function at the start and at the end, i.e. by assuming that the 
! value of the level function at a point varies linearly with time.
!
! ***NOTE: If a narrow band scheme is used to advance the level function in time
! then the firelines at time_start and time_end must fit within the band 
! at either time, and level function values away from the band should be set
! to some large positive and negative constants to assure that the ignition times
! in the area between the firelines are set reasonably. This is also important 
! when the level function is used for data assimilation. ***
!
! It is the responsibility of the caller to:
!
! before the call
!    - polulate all cells with the proper fuel_time coefficient
!    - interpolate and correct atmospheric winds
!
! after the call
!    - compute the fluxes from the fuel fraction burned
!      and sum up the the fluxes over atmopheric grid cells
!
!*** local
real, dimension(ims:ime-1,jms:jme-1)::frac_start
real, dimension(ims:ime,jms:jme)::phi_start
real::dummy
integer::i,j
real, dimension(ids:ide,jds:jde)::phi_d,vx_d,vy_d,r_d


!*** executable

! compute the fuel fraction at time_start
call fuel_left(ids,ide,jds,jde,ims,ime,jms,jme, &
    phi,tign,fuel_time,time_start,frac_start)

! save the level function for interpolation between the old and the new one later
do i=ids,ide
    do j=jds,jde
        phi_start(i,j)=phi(i,j)
    enddo
enddo 

! advance the level function from time_start to time_end
! prop_ls will be replaced anyway, do not bother adding overlaps
phi_d=phi(ids:ide,jds:jde)
vx_d=vx(ids:ide,jds:jde)
vy_d=vy(ids:ide,jds:jde)
r_d=r(ids:ide,jds:jde)
call prop_ls(ids,ide,jds,jde,phi_d,dummy,time_start,time_end,vx_d,vy_d,r_d,fire_dx,fire_dy)
phi(ids:ide,jds:jde)=phi_d

! set ignition time at the nodes the fireline moved over
do j=jds,jde
    do i=ids,ide
        if(phi_start(i,j)>0 .and. .not. phi(i,j)>0)then
            ! node was not burning at start but it is burning at end
            ! interpolate from the level functions at start and at end
        ! phi_start(i,j) is the level function value at time_start
            ! phi(i,j)       is the level function value at time_end
            ! 0              is the level function value at tign(i,j)
            tign(i,j)=time_start + (time_end-time_start) &
                         * phi_start(i,j) / (phi_start(i,j) - phi(i,j))
        endif
    enddo
enddo 

! compute the fuel fraction at time_end 
call fuel_left(ids,ide,jds,jde,ims,ime,jms,jme, &
    phi,tign,fuel_time,time_end,frac_end)

! compute the fuel fraction lost
do j=jds,jde-1
    do i=ids,ide-1
        frac_lost(i,j)=frac_end(i,j)-frac_start(i,j)
    enddo
enddo

end subroutine fire_spread_core

end module module_fr_spread_core
