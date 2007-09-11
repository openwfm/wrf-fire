!
!*** Jan Mandel August 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!

module module_fr_sfire_core
contains

subroutine sfire_core( ids,ide,jds,jde,  &
                             ims,ime,jms,jme,  &
                             time_start,time_diff,fire_dx,fire_dy,fuel_time,   &
                             phi,tign,fuel_frac,fuel_frac_burnt ) 

!*** purpose
!
! Dynamical core of the fire spread model, insulated from the physics. 
! References the fire grid ONLY. Keep separated from the coupling
! with atm. model and data input.
!

use module_fr_sfire_burn
use module_fr_sfire_prop
use module_fr_sfire_util
implicit none

!*** arguments

integer, intent(in) :: ids,ide,jds,jde,ims,ime,jms,jme
real, intent(inout), dimension(ims:ime,jms:jme)::phi,tign,fuel_frac,fuel_time,fuel_frac_burnt
real, intent(in):: time_start,time_diff,fire_dx,fire_dy

! argument          intent  description                     (unit)  lives at
!
! ids,ide,jds,jde   in   mesh domain dimensions (cells)      (1)
! ims,ime,jms,jme   in   mesh aray dimensions                (1)
! time_start        in   the starting time                   (s)
! time_diff          in   the length of simulation            (s)
! fire_dx,fire_dy   in   fire mesh spacings                  (m)
! fuel_time         in   time fuel burns down to 1/e         (s)      cells
! phi             inout  level function (state)              (1)      nodes
! tign            inout  ignition time (state)               (s)      nodes
! fuel_frac       inout  the fuel fraction at the end        (1)      cells
! fuel_frac_burnt   out  the fuel fraction burnt this call   (1)      cells 

! A speed function is called for the spread rate at selected points in 
! module_fr_sfire_prop. 
! fuel_frac is currently ignored on input and computed from tign but it may not
! be in future.

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
real, dimension(ims:ime,jms:jme)::frac_start
integer::i,j


!*** executable

call check_mesh_dim(ids,ide+1,jds,jde+1,ims,ime,jms,jme)

! compute the fuel fraction at time_start
call fuel_left(ids,ide,jds,jde,ims,ime,jms,jme, &
    phi,tign,fuel_time,time_start,frac_start)

! propagate the fireline: advance the level function 
! from time_start to time_start + time_diff
call prop_ls &
  (ids,ide,jds,jde,ims,ime,jms,jme,phi,tign,time_start,time_diff,fire_dx,fire_dy)

! compute the fuel fraction at time_end 
call fuel_left(ids,ide,jds,jde,ims,ime,jms,jme, &
    phi,tign,fuel_time,time_start+time_diff,fuel_frac)

! compute the fuel fraction lost
do j=jds,jde
    do i=ids,ide
        fuel_frac_burnt(i,j)=fuel_frac(i,j)-frac_start(i,j)
    enddo
enddo

end subroutine sfire_core

end module module_fr_sfire_core
