!
!*** Jan Mandel August-October 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
! With contributions by Minjeong Kim.
#define DEBUG_OUT

module module_fr_sfire_core

use module_fr_sfire_util

! The mathematical core of the fire spread model. No physical constants here.
! 
! subroutine sfire_core: only this routine should be called from the outside.
! subroutine fuel_left:  compute remaining fuel from time of ignition.
! subroutine prop_ls: propagation of curve in normal direction.

contains

subroutine sfire_core(       ids,ide,jds,jde,  &
                             ims,ime,jms,jme,  &
                             num_tiles,i_start,i_end,j_start,j_end, & ! tiling
                             time_start,time_diff,fire_dx,fire_dy,fuel_time,   &
                             lfn,tign,fuel_frac,fuel_frac_burnt &
#ifdef SPEED_VARS_ARGS      /* extra arguments for normal_spread */
#include SPEED_VARS_ARGS
#else
#include "fr_sfire_params_args.h"
#endif
                             ) 

!*** purpose
!
! Dynamical core of the fire spread model, insulated from the physics. 
! References the fire grid ONLY. Keep separated from the coupling
! with atm. model and data input.
!

implicit none

!*** arguments

integer, intent(in) :: ids,ide,jds,jde,ims,ime,jms,jme
integer,intent(in)::num_tiles
integer,intent(in),dimension(num_tiles)::i_start,i_end,j_start,j_end
real, intent(inout), dimension(ims:ime,jms:jme)::lfn,tign,fuel_frac,fuel_time,fuel_frac_burnt
real, intent(in):: time_start,time_diff,fire_dx,fire_dy
#ifdef SPEED_VARS_DECL      /* extra arguments for normal_spread */
#include SPEED_VARS_DECL
#else
#include "fr_sfire_params_decl.h"
#endif


! argument          intent  description                     (unit)  lives at
!
! ids,ide,jds,jde   in   mesh domain dimensions (cells)      (1)
! ims,ime,jms,jme   in   mesh aray dimensions                (1)
! time_start        in   the starting time                   (s)
! time_diff          in   the length of simulation            (s)
! fire_dx,fire_dy   in   fire mesh spacings                  (m)
! fuel_time         in   time fuel burns down to 1/e         (s)      cells
! avx,avy             in    background advection speed
! sr                 in    background spread rate in normal direction
! lfn             inout  level function (state)              (1)      nodes
! tign            inout  ignition time (state)               (s)      nodes
! fuel_frac       inout  the fuel fraction at the end        (1)      cells
! fuel_frac_burnt   out  the fuel fraction burnt this call   (1)      cells 

! A speed function is called for the spread rate at selected points.
! fuel_frac is currently ignored on input and computed from tign but it may not
! be in future.

!*** description

! This is a dynamical core of the fire spread model, insulated from the physics. 
! The physics should be done in the pre- and postprocessing, and in 
! the speed function for the fireline propagation (future).
!
! The state of the model is the level function lfn, which determines the fire 
! area, and the ignition time tign, both interpolated from values at nodes.
! The fire area is the level set where lfn <= 0.  The fireline is where lfn=0. 
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
!double precision frac_start(ims:ime,jms:jme)
real:: time_now
integer::i,j,fuel_left_type

!*** calls
intrinsic epsilon


!*** executable

call check_mesh_2dim(ids,ide+1,jds,jde+1,ims,ime,jms,jme)
call check_tiles(ids,ide,jds,jde,num_tiles,i_start,i_end,j_start,j_end)

fuel_left_type=3

! compute the fuel fraction at time_start - not needed if passed in
if (fuel_left_type .eq. 1)then
    call fuel_left(ids,ide,jds,jde,ims,ime,jms,jme, &
    lfn,tign,fuel_time,time_start,frac_start)
else if (fuel_left_type .eq. 2) then
    call fuel_left_jm(ids,ide,jds,jde,ims,ime,jms,jme, &
    lfn,tign,fuel_time,time_start,frac_start)
else if (fuel_left_type .eq. 3) then
    call fuel_burnt(ids,ide,jds,jde,fire_dx,fire_dy,ims,ime,jms,jme,&
    lfn,tign,fuel_time,time_start,frac_start)        
endif

! propagate the fireline: advance the level function 
! from time_start to time_start + time_diff
call prop_ls(      ids,ide,jds,jde, & ! dims
                   ims,ime,jms,jme, & ! dims
                   num_tiles,i_start,i_end,j_start,j_end, &
                   time_start,time_diff,fire_dx,fire_dy,     & ! scalars in
                   lfn,tign &            ! arrays out
#                  ifdef SPEED_VARS_ARGS      /* extra arguments for normal_spread */
#                        include SPEED_VARS_ARGS
#              else
#                        include "fr_sfire_params_args.h"
#                  endif
             )

! compute the fuel fraction at time_end 
if (fuel_left_type .eq. 1)then
    call fuel_left(ids,ide,jds,jde,ims,ime,jms,jme, &
    lfn,tign,fuel_time,time_start+time_diff,fuel_frac)
else if (fuel_left_type .eq. 2) then
    call fuel_left_jm(ids,ide,jds,jde,ims,ime,jms,jme, &
    lfn,tign,fuel_time,time_start+time_diff,fuel_frac)
else if (fuel_left_type .eq. 3) then
    call fuel_burnt(ids,ide,jds,jde,fire_dx,fire_dy,ims,ime,jms,jme,&
    lfn,tign,fuel_time,time_start+time_diff,fuel_frac)        
endif

! compute the fuel fraction lost
do j=jds,jde
    do i=ids,ide
        fuel_frac_burnt(i,j)=frac_start(i,j)-fuel_frac(i,j)
    enddo
enddo

call print_2d_stats(ids,ide,jds,jde,ims,ime,jms,jme,fuel_frac_burnt,'core: fuel frac burned (1)')

! set the ignition time outside of burning region to now
! this is consistent with the fuel fraction
time_now = time_start + time_diff
time_now = time_now + abs(time_now)*epsilon(time_now)*2.
do j=jds,jde
    do i=ids,ide
        if(lfn(i,j)>0.)tign(i,j)=time_now
    enddo
enddo

end subroutine sfire_core

!
!****************************************
!

subroutine init_no_fire     (ifds,ifde,jfds,jfde, & ! dimensions
                             ifts,ifte,jfts,jfte, &
                             ifms,ifme,jfms,jfme, &
                             fdx,fdy,time_now,    & ! scalars in
                             fuel_frac,lfn,tign)    ! arrays out            
implicit none
             
!*** purpose: initialize model to no fire

!*** arguments
integer, intent(in):: ifds,ifde,jfds,jfde   ! fire domain bounds
integer, intent(in):: ifts,ifte,jfts,jfte   ! fire tile bounds
integer, intent(in):: ifms,ifme,jfms,jfme   ! array bounds
real, intent(in) :: fdx,fdy,time_now        ! mesh spacing, time
real, intent(out), dimension (ifms:ifme,jfms:jfme) :: & 
                   fuel_frac,lfn,tign       ! model state

!*** calls
intrinsic epsilon
                                                
!*** local
integer:: i,j
real lfn_init,time_init


do j=jfts,jfte
    do i=ifts,ifte
        fuel_frac(i,j)=1.  ! fuel at start is 1 by definition
    enddo
enddo

lfn_init = 2*max((ifde-ifds+1)*fdx,(jfde-jfds+1)*fdy)      ! more than domain diameter
time_init=time_now + max(time_now,1.0)*epsilon(time_now) ! a bit in future
 
do j=jfts,jfte+1
    do i=ifts,ifte+1
        tign(i,j) = time_init      ! ignition in future
        lfn(i,j) = lfn_init        ! no fire 
    enddo
enddo
call message('init_model_no_fire: state set to no fire')

end subroutine init_no_fire

!
!******************
!
 

subroutine ignite_fire( ifds,ifde,jfds,jfde,                    & ! fire domain dims - the whole domain
                        ifts,ifte,jfts,jfte,                      &
                        ifms,ifme,jfms,jfme,                      &
                        sx,sy,ex,ey,r,time_ign,fdx,fdy,           &
                        lfn,tign,ignited)
implicit none

!*** purpose: ignite a circular fire 

!*** arguments
integer, intent(in):: ifds,ifde,jfds,jfde   ! fire domain bounds
integer, intent(in):: ifts,ifte,jfts,jfte   ! fire tile bounds
integer, intent(in):: ifms,ifme,jfms,jfme   ! array bounds
real, intent(in):: time_ign                 ! the ignition time of the fire
real, intent(in):: sx,sy                    ! start of ignition line, from lower left corner
real, intent(in):: ex,ey                    ! end of ignition line, or zero
real, intent(in):: r                        ! all within the radius of the line will ignite
real, intent(in):: fdx,fdy                  ! mesh spacing (m)
real, intent(inout), dimension (ifms:ifme,jfms:jfme) :: & 
                   lfn, tign                ! level function, ignition time (state)
integer, intent(out):: ignited              ! number of nodes newly ignited
                        
!*** local
integer:: i,j
real::mx,my,ax,ay,dam2,d,dames,des2,am_es,cos2,lfn_new,dmc2
logical::point
character(len=128):: msg

ignited=0
point = ex .eq. 0.0 .or. ey .eq. 0.0
if (.not.point)then
    ! midpoint m = (mx,my)
    mx = (sx + ex)/2
    my = (sy + ey)/2
else    
    mx = sx
    my = sy
endif
do j=jfts,jfte+1      ! node based loops, hence the +1
    do i=ifts,ifte+1
        ! coordinates of the point a=(ax ay), the lower left corner of the domain is (0 0)
        ax = fdx*(i - ifds)
        ay = fdy*(j - jfds)
        dam2=(ax-mx)*(ax-mx)+(ay-my)*(ay-my)      ! |a-m|^2
        if(point)then
            d=sqrt(dam2)
        else
            ! compute distance as distance from midpoint minus correction
            ! |a-c|^2 = |a-m|^2 - |m-c|^2
            ! when |m-c| >= |s-e|/2 use distance from the endpoint instead
            !
            !           a    
            !          /| \ 
            !     s---m-c--e
            !
            ! |m-c| = |a-m| cos (a-m,e-s) 
            !       = |a-m| (a-m).(e-s))/(|a-m|*|e-s|)
            des2 = (ex-sx)*(ex-sx)+(ey-sy)*(ey-sy)          ! |e-s|^2
            dames = dam2*des2
            if(dames>0)then
                am_es=(ax-mx)*(ex-sx)+(ay-my)*(ey-sy)       ! (a-m).(e-s)
                cos2 = (am_es*am_es)/dames                  ! cos^2 (a-m,e-s)
            else
                cos2 = 0.
            endif
            dmc2 = dam2*cos2                                ! |m-c|^2
            if(4.*dmc2 <= des2)then
                d = sqrt(max(dam2 - dmc2,0.))               ! just in case, rounding
            elseif(am_es>0)then                             ! cos > 0, closest is e
                d = sqrt((ax-ex)*(ax-ex)+(ay-ey)*(ay-ey))   ! |a-e|
            else
                d = sqrt((ax-sx)*(ax-sx)+(ay-sy)*(ay-sy))   ! |a-s|
            endif
        endif
        lfn_new=d-r
        if(lfn(i,j)>0 .and. lfn_new<=0) then
            tign(i,j)=time_ign  ! newly ignited now
            ignited=ignited+1   ! count
        endif
        lfn(i,j)=min(lfn(i,j),lfn_new)  ! update the level set function
    enddo
enddo
write(msg,'(a,2f10.1,a,2f10.1,a,f8.1,a,f8.1,a,i3)')'ignite_fire: from',sx,sy,' to ',&
    ex,ey,' radius ',r,' time',time_ign,' ignited nodes',ignited
call message(msg)
end subroutine ignite_fire

!
!**********************
!            

subroutine fuel_left_jm(ids,ide,jds,jde, &
                     ims,ime,jms,jme, &
                     lfn, tign, fuel_time, tnow, fuel_frac)
implicit none

!*** purpose: determine fraction of fuel remaining

!*** Jan Mandel August 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com

!*** arguments

integer, intent(in) :: ids,ide,jds,jde,ims,ime,jms,jme
real, intent(in), dimension(ims:ime,jms:jme)::lfn,tign,fuel_time
real, intent(in):: tnow
real, intent(out), dimension(ims:ime,jms:jme)::fuel_frac

! ids,ide,jds,jde   in   mesh domain dimensions (cells)      (1)
! ims,ime,jms,jme   in   mesh aray dimensions                (1)
! lfn               in   level function, at nodes
! tign              in   ignition time, at nodes
! fuel_time         in   time constant of fuel, per cell
! tnow              in   time now
! fuel_frac         out  fraction of fuel remaining, per cell

!*** Description
! The area burning is given by the condition P(x,y) <= 0, where the function P is
! interpolated from the values of lfn at mesh nodes,
! P(dx*(i-1),dy*(j-1))=lfn(i,j).
!
! The time since ignition in location (x,y) is the function T, interpolated in 
! each mesh cell from the values T(dx*(i-1),dy*(j-1))=tign(i,j) at the nodes
! where lfn(i,j)<=0, and T(x,y)=tnow on all points on the grid lines where P(x,y) = 0.
! The values of tign(i,j) where lfn(i,j)>0 are ignored.
!
! The subroutine computes for each mesh cell [dx*(i-1),dx*i] by [dy*(j-1),dy*j]
! an approximation of the average of exp(-T(x,y)/fuel_time(i,j)) over the burning area
! in the cell, that is an approximation of the integral
!
!                              /\
!                     1        |            T(x,y)-now
! fuel_frac(i,j)  =  -----     |    exp( - ------------- ) dxdy
!                    dx*dy     |           fuel_time(i,j)
!                             \/
!                       dx*(i-1)<x<dx*i
!                       dy*(j-1)<y<dy*j
!                        lfn(x,y)<=0
!
! When the cell is not burning at all (all lfn>=0), then fuel_frac(i,j)=1.
! Because of symmetries, the result should not depend on the mesh spacing dx dy
! so dx and dy are not in the argument list.
!
! Example:
!
!        lfn<0         lfn>0
!      (i,j+1)-----O--(i+1,j+1)            O = points on the fireline, T=tnow
!            |      \ |                    A = the burning area for computing
!            |       \|                        fuel_frac(i,j)
!            |   A    O 
!            |        |
!            |        |
!       (i,j)---------(i+1,j)
!       lfn<0          lfn<0
!
! Approximations allowed: 
! The fireline can be approximated by straight line(s).
! When all cell is burning, approximation by 1 point Gaussian quadrature is OK.
! 
! Requirements:
! 1. The output should be a continuous function of the arrays lfn and
!  tign whenever lfn(i,j)=0 implies tign(i,j)=tnow.  
! 2. The output should be invariant to the symmetries of the input in each cell.
! 3. Arbitrary combinations of the signs of lfn(i,j) should work.
! 4. The result should be at least 1st order accurate in the sense that it is
!    exact if the time from ignition is a linear function.
!
! Note: If time from ignition is approximated by polynomial in the burnt
! region of the cell, this is integral of polynomial times exponential
! over a polygon, which can be computed exactly.
!
! Requirement 4 is particularly important when there is a significant decrease
! of the fuel fraction behind the fireline on the mesh scale, because the
! rate of fuel decrease right behind the fireline is much larger 
! (exponential...). This will happen when
! 
!      change of time from ignition within one mesh cell
! X =  --------------------------------------------------  is not << 1
!                     fuel_time.
!
! (The code should compute and print this quantitity, max min avg)
!
! This is the same as
!
!               mesh cell size
!         --------------------------   is not << 1
!         fuel_time * fireline speed
!         
! (This subroutine does not have that data so it cannot compute and print this).
!
! When X is large then the fuel burnt in one timestep in the cell is
! approximately proportional to length of  fireline in that cell.
!
! When X is small then the fuel burnt in one timestep in the cell is
! approximately proportional to the area of the burning region.
!

!
!
! IMPORTANT: follow WRF coding conventions 
! http://www.mmm.ucar.edu/wrf/WG2/WRF_conventions.html

!*** local

integer::i,j
real,dimension(ims:ime,jms:jme)::t,ap
real:: ta,aps,ps,area

! a better approximation - replace by a still better code

call check_mesh_2dim(ids,ide+1,jds,jde+1,ims,ime,jms,jme)

! precompute nodal quantities
do j=jds,jde+1     ! note the order of indices for fast  execution
    do i=ids,ide+1  
        ap(i,j)=abs(lfn(i,j))
        ! scaled time since ignition         
        if (lfn(i,j)>0) then
            t(i,j)=0.
        else
            t(i,j)=tnow - tign(i,j)
        endif
    enddo
enddo

do j=jds,jde
    do i=ids,ide ! it is OK to introduce extra scalars, just as fast

        ! approximate relative burning area, between 0 and 1   
        ps=lfn(i+1,j+1)+lfn(i+1,j)+lfn(i,j+1)+lfn(i,j)   
        aps=ap(i+1,j+1)+ ap(i+1,j)+ ap(i,j+1)+ ap(i,j)
        area=(-ps/aps+1.)*.5
    
        ! average time since ignition
        ta=0.25*(t(i+1,j+1)+t(i+1,j)+t(i,j+1)+t(i,j))

        ! exp decay in the burning area
        fuel_frac(i,j)=area*exp(-ta/fuel_time(i,j)) + (1. - area)
        
        ! note: this is exact in the limit 
        ! fuel_time/ta= fuel_time*fire_speed/cell_size -> 0

    enddo
enddo

end subroutine fuel_left_jm

!
!****************************************
!

subroutine fuel_left(ids,ide,jds,jde, &
                     ims,ime,jms,jme, &
                     lfn, tign, fuel_time, tnow, fuel_frac)
implicit none

!*** purpose: determine fraction of fuel remaining

!*** Jan Mandel August 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com

!*** arguments

integer, intent(in) :: ids,ide,jds,jde,ims,ime,jms,jme
real, intent(in), dimension(ims:ime,jms:jme)::lfn,tign,fuel_time
real, intent(in):: tnow
real, intent(out), dimension(ims:ime,jms:jme)::fuel_frac

! ids,ide,jds,jde   in   mesh domain dimensions (cells)      (1)
! ims,ime,jms,jme   in   mesh aray dimensions                (1)
! lfn               in   level function, at nodes
! tign              in   ignition time, at nodes
! fuel_time         in   time constant of fuel, per cell
! tnow              in   time now
! fuel_frac         out  fraction of fuel remaining

!*** Description
! The area burning is given by the condition P(x,y) <= 0, where the function P is
! interpolated from the values of lfn at mesh nodes,
! P(dx*(i-1),dy*(j-1))=lfn(i,j).
!
! The time since ignition in location (x,y) is the function T, interpolated in 
! each mesh cell from the values T(dx*(i-1),dy*(j-1))=tign(i,j) at the nodes
! where lfn(i,j)<=0, and T(x,y)=tnow on all points on the grid lines where P(x,y) = 0.
! The values of tign(i,j) where lfn(i,j)>0 are ignored.
!
! The subroutine computes for each mesh cell [dx*(i-1),dx*i] by [dy*(j-1),dy*j]
! an approximation of the average of exp(-T(x,y)/fuel_time(i,j)) over the burning area
! in the cell, that is an approximation of the integral
!
!                              /\
!                     1        |            T(x,y)-tnow
! fuel_frac(i,j)  =  -----     |    exp( - ------------- ) dxdy
!                    dx*dy     |           fuel_time(i,j)
!                             \/
!                       dx*(i-1)<x<dx*i
!                       dy*(j-1)<y<dy*j
!                        lfn(x,y)<=0
!
! When the cell is not burning at all (all lfn>=0), then fuel_frac(i,j)=1.
! Because of symmetries, the result should not depend on the mesh spacing dx dy
! so dx and dy are not in the argument list.
!
! Example:
!
!        lfn<0         lfn>0
!      (i,j+1)-----O--(i+1,j+1)            O = points on the fireline, T=tnow
!            |      \ |                    A = the burning area for computing
!            |       \|                        fuel_frac(i,j)
!            |   A    O 
!            |        |
!            |        |
!       (i,j)---------(i+1,j)
!       lfn<0          lfn<0
!
! Approximations allowed: 
! The fireline can be approximated by straight line(s).
! When all cell is burning, approximation by 1 point Gaussian quadrature is OK.
! 
! Requirements:
! 1. The output should be a continuous function of the arrays lfn and
!  tign whenever lfn(i,j)=0 implies tign(i,j)=tnow.  
! 2. The output should be invariant to the symmetries of the input in each cell.
! 3. Arbitrary combinations of the signs of lfn(i,j) should work.
! 4. The result should be at least 1st order accurate in the sense that it is
!    exact if the time from ignition is a linear function.
!
! Note: If time from ignition is approximated by polynomial in the burnt
! region of the cell, this is integral of polynomial times exponential
! over a polygon, which can be computed exactly.
!
! Requirement 4 is particularly important when there is a significant decrease
! of the fuel fraction behind the fireline on the mesh scale, because the
! rate of fuel decrease right behind the fireline is much larger 
! (exponential...). This will happen when
! 
!      change of time from ignition within one mesh cell
! X =  --------------------------------------------------  is not << 1
!                     fuel_time.
!
! (The code should compute and print this quantitity, max min avg)
!
! This is the same as
!
!               mesh cell size
!         --------------------------   is not << 1
!         fuel_time * fireline speed
!         
! (This subroutine does not have that data so it cannot compute and print this).
!
! When X is large then the fuel burnt in one timestep in the cell is
! approximately proportional to length of  fireline in that cell.
!
! When X is small then the fuel burnt in one timestep in the cell is
! approximately proportional to the area of the burning region.
!

!
!
! IMPORTANT: follow WRF coding conventions 
! http://www.mmm.ucar.edu/wrf/WG2/WRF_conventions.html

!*** local

integer::i,j
real,dimension(ims:ime,jms:jme)::t,ap
real:: ta,aps,ps,a

! a very crude approximation - replace by a better code

call check_mesh_2dim(ids,ide+1,jds,jde+1,ims,ime,jms,jme)

! t = fuel fraction at node
do j=jds,jde+1     ! note the order of indices for fast  execution
    do i=ids,ide+1  
        if (lfn(i,j)>0) then
            t(i,j)=1.0  ! add missing values as all fuel
        else
            ! ignore ignition times in future
            t(i,j)=exp(min(tign(i,j)-tnow,0.)/fuel_time(i,j))
        endif
        ap(i,j)=abs(lfn(i,j))
    enddo
enddo

do j=jds,jde
    do i=ids,ide ! it is OK to introduce extra scalars, just as fast
    
        ! average fuel fraction since ignition
        ta=0.25*(t(i+1,j+1)+t(i+1,j)+t(i,j+1)+t(i,j))  
        ps=lfn(i+1,j+1)+lfn(i+1,j)+lfn(i,j+1)+lfn(i,j)   
        aps=ap(i+1,j+1)+ ap(i+1,j)+ ap(i,j+1)+ ap(i,j)
        
        ! a=0 if all lfn>0, 1 if all <0, transition except when all lfn=0
        if (aps>0.0 .or. aps<0.0) then  ! never compare =0.0, slow
               a=(-ps/aps+1.0)*0.5
        else ! for fast code, the else clause should happen less often
               a=0.5d0 ! just to have something, if all lfn=0 it is junk anyway
        endif

        fuel_frac(i,j)=a*ta+(1.0-a)

    enddo
enddo

end subroutine fuel_left

!
!****************************************
!
subroutine fuel_burnt(ids,ide,jds,jde,dx,dy,&
                     ims,ime,jms,jme, &
                     lfn, tign, fuel_time, tnow, fuel_burn)                     
implicit none
!*** arguments
integer, intent(in) :: ids,ide,jds,jde,ims,ime,jms,jme
real, intent(in), dimension(ims:ime,jms:jme)::lfn,tign,fuel_time
real, intent(in):: tnow,dx,dy
real, intent(out), dimension(ims:ime,jms:jme)::fuel_burn

! ids,ide,jds,jde   in   mesh domain dimensions (cells)      (1)
! ims,ime,jms,jme   in   mesh aray dimensions                (1)
! lfn               in   level function, at nodes
! tign              in   ignition time, at nodes
! fuel_time         in   time constant of fuel, per cell
! tnow              in   time now
! fuel_frac         out  fraction of fuel remaining

!*** local
integer::i,j,k

! least squares
integer::mmax,nb,nmax,pmax,nin,nout
parameter(mmax=3,nb=64,nmax=8,pmax=8)
integer lda, ldb, lwork, info
parameter (lda=nmax, ldb=nmax, lwork=mmax+nmax+nb*(nmax+pmax))
integer n,m,p
!double precision mA(lda,mmax), vecD(nmax), &
!                 WORK(lwork), vecY(pmax),vecX(mmax)
real,dimension(lda,mmax):: mA
real,dimension(nmax):: vecD
real,dimension(lwork):: WORK
real,dimension(pmax):: vecY
real,dimension(mmax):: vecX
real,dimension(ldb,pmax)::mB
real,dimension(mmax)::u

real::tweight,tdist
integer::kk,ll,ss
real::rnorm
real,dimension(8,2)::xylist,xytlist
real,dimension(8)::tlist,llist,xt
real,dimension(5)::xx,yy
integer,dimension(5)::ii,jj
!ii,jj data here
data (ii(i),i=1,5)/1,2,2,1,1/
data (jj(i),i=1,5)/1,1,2,2,1/
integer:: npoint
real::tt,x0,y0,xts,xte,yts,yte,xt1,xt2
real::lfn0,lfn1,dist,nr,c,s,errQ,ae,ce,ceae,a0,a1,a2,d
real::s1,s2,s3
real::upper,lower,ah,ch,aa,cc,aupp,cupp,alow,clow
real,dimension(2,2)::mQ
real,dimension(2)::ut

!calls
intrinsic epsilon

real, parameter:: zero=0.,one=1.,eps=epsilon(zero)


! external functions    
real::dnrm2
external dnrm2
! external subroutines
!external dggglm
external sggglm
external dgemv
external dgemm
!executable statements

!real::eps
!eps = 2.2204*(10.0**(-8))!from matlab

! a very crude approximation - replace by a better code
call check_mesh_2dim(ids,ide+1,jds,jde+1,ims,ime,jms,jme)       

do j=jds,jde      ! cell by cell
    do i=ids,ide  ! [dx*(i-1),dx*i] by [dy*(j-1),dy*j]
        !*** case0 Do nothing
        if ( lfn(i,j)>=0 .and. lfn(i+1,j)>=0 .and. &
             lfn(i,j+1)>=0  .and. lfn(i+1,j+1)>=0 ) then
            fuel_burn(i,j)= 0.0 !  fuel_burnt
        !*** case4 all four coners are burning
        else if ( lfn(i,j)<=0 .and. lfn(i+1,j)<=0 .and. &
                  lfn(i,j+1)<=0  .and. lfn(i+1,j+1)<=0 ) then
            ! least squares, A matrix for points
            mA(1,1)=0.0
            mA(2,1)=dx
            mA(3,1)=0.0
            mA(4,1)=dx
            mA(1,2)=0.0
            mA(2,2)=0.0
            mA(3,2)=dy
            mA(4,2)=dy
            mA(1,3)=1.0
            mA(2,3)=1.0
            mA(3,3)=1.0
            mA(4,3)=1.0
            ! D vector, time from ignition
            vecD(1)=tnow-tign(i  ,j  )
            vecD(2)=tnow-tign(i+1,j  )
            vecD(3)=tnow-tign(i  ,j+1)
            vecD(4)=tnow-tign(i+1,j+1)
            ! B matrix, weights
            do kk=1,4
            do ll=1,4
              mB(kk,ll)=0.0
            end do
            mB(kk,kk)=2.0
            end do
            ! set the m,n,p
            n=4 ! rows of matrix A and B
            m=3 ! columns of matrix A
            p=4 ! columns of matrix B
            ! call least squqres in LAPACK            
            call SGGGLM(N,M,P,mA,LDA,mB,LDB,vecD,vecX,vecY, &
                        WORK,LWORK,INFO)
            rnorm=dnrm2(p,vecY,1)            
            ! integrate
            u(1)=-vecX(1)/fuel_time(i,j)
            u(2)=-vecX(2)/fuel_time(i,j)
            u(3)=-vecX(3)/fuel_time(i,j)            
            !fuel_burn(i,j)=1-exp(u(3))*intexp(u(1)*dx)*intexp(u(2)*dy)
            s1=u(1)*dx
            s2=u(2)*dy            
            fuel_burn(i,j)=1-exp(u(3))*intexp(s1)*intexp(s2)
            !print *,'intexp
            if ( fuel_burn(i,j)<0 .or. fuel_burn(i,j)>1.0 ) then
                print *,'case4, fuel_burnt should be between 0 and 1'
            end if
        !*** case 1,2,3
        else
            ! set xx, yy for the coner points
            ! move these values out of i and j loop to speed up
            xx(1) = -dx/2
            xx(2) = dx/2
            xx(3) = dx/2
            xx(4) = -dx/2
            xx(5) = -dx/2
            yy(1) = -dy/2
            yy(2) = -dy/2
            yy(3) = dy/2
            yy(4) = dy/2
            yy(5) = -dy/2       
        
            npoint = 0 ! number of points in polygon
            do k=1,4
                lfn0=lfn(i+ii(k  )-1,j+jj(k  )-1)
                lfn1=lfn(i+ii(k+1)-1,j+jj(k+1)-1)
                if ( lfn0 <= 0.0 ) then
                    npoint = npoint + 1
                    xylist(npoint,1)=xx(k)
                    xylist(npoint,2)=yy(k)
                    tlist(npoint)=tnow-tign(ii(k),jj(k))
                    llist(npoint)=lfn0
                end if
                if ( lfn0*lfn1 < 0 ) then
                    npoint = npoint + 1
                    tt=lfn0/(lfn0-lfn1)
                    x0=xx(k)+( xx(k+1)-xx(k) )*tt
                    y0=yy(k)+( yy(k+1)-yy(k) )*tt
                    xylist(npoint,1)=x0
                    xylist(npoint,2)=y0
                    tlist(npoint)=0 ! on fireline
                    llist(npoint)=0
                end if
            end do
            ! make the list circular
            tlist(npoint+1)=tlist(1)
            llist(npoint+1)=llist(1)   
            xylist(npoint+1,1)=xylist(1,1)
            xylist(npoint+1,2)=xylist(1,2)
                      
            !* least squares, A matrix for points
            do kk=1,npoint
              mA(kk,1)=xylist(kk,1)
              mA(kk,2)=xylist(kk,2)
              mA(kk,3)=1.0
              vecD(kk)=tlist(kk) ! D vector,time from ignition
            end do            
            ! B matrix, weights
            do kk=1,ldb
            do ll=1,pmax
              mB(kk,ll)=0.0 ! clear
            end do
            end do
        
            do kk=1,npoint
              mb(kk,kk)=dx ! large enough
              do ll=1,npoint
                if ( kk .ne. ll ) then
                  dist = sqrt( (xylist(kk,1)-xylist(ll,1))**2+ &
                               (xylist(kk,2)-xylist(ll,2))**2 )                   
                  mB(kk,kk)=min( mB(kk,kk) , dist )
                end if              
              end do !ll
              mB(kk,kk)=mB(kk,kk)+1.
            end do ! kk
            ! set the m,n,p
            n=npoint ! rows of matrix A and B
            m=3 ! columns of matrix A
            p=npoint ! columns of matrix B
            !* call least squqres in LAPACK                  
            call SGGGLM(N,M,P,mA,LDA,mB,LDB,vecD,vecX,vecY, &
                        WORK,LWORK,INFO)
            rnorm=dnrm2(p,vecY,1)
            u(1)=vecX(1)
            u(2)=vecX(2)
            u(3)=vecX(3)            
            ! rotate to gradient on x only
            nr = sqrt(u(1)**2+u(2)**2)
            c = u(1)/nr
            s = u(2)/nr
            mQ(1,1)=c
            mQ(1,2)=s
            mQ(2,1)=-s
            mQ(2,2)=c            
            ! mat vec multiplication
            call matvec(mQ,2,2,u,3,ut,2,2,2)            
            errQ = ut(2) ! should be zero            
            ae = -ut(1)/fuel_time(i,j)
            ce = -u(3)/fuel_time(i,j)
            
            ! mat mat' multiplication
            !call DGEMM('N','T',npoint+1,2,2,1.0,xylist,8,mQ,2,0.0,xytlist,npoint+1)
            !(A,mA,nA,B,mB,nB,C,mC,nC,nrow,ncolumn,nP)            
            call matmatp(xylist,8,2,mQ,2,2,xytlist,8,2,npoint+1,2,2)            
            call sortxt( xytlist, 8,2, xt,8,npoint )            
            fuel_burn(i,j)=0.0
            aupp=0.0
            cupp=0.0
            alow=0.0
            clow=0.0
            do k=1,npoint-1
              xt1=xt(k)
              xt2=xt(k+1)
              upper=0
              lower=0
              ah=0
              ch=0
              if ( xt2-xt1 > eps*dx*100 ) then
                
                do ss=1,npoint
                  xts=xytlist(ss,1)
                  yts=xytlist(ss,2)
                  xte=xytlist(ss+1,1)
                  yte=xytlist(ss+1,2)
                  
                  if ( (xts>xt1 .and. xte>xt1) .or. &
                       (xts<xt2 .and. xte<xt2) ) then
                    aa = 0 ! do nothing
                    cc = 0
                  else
                    aa = (yts-yte)/(xts-xte)
                    cc = (xts*yte-xte*yts)/(xts-xte)                    
                    if (xte<xts) then
                      aupp = aa
                      cupp = cc
                      ah=ah+aa
                      ch=ch+cc
                      upper=upper+1
                    else
                      alow = aa
                      clow = cc
                      lower=lower+1
                    end if
                  end if!(xts>xt1 .and. xte>xt1)              
                end do ! ss
                ah = aupp-alow
                ch = cupp-clow                
                ! integrate (ah*x+ch)*(1-exp(ae*x+ce) from xt1 to xt2
                ! numerically sound for ae->0, ae -> infty
                ! this can be important for different model scales
                ! esp. if someone runs the model in single precision!!
                ! s1=int((ah*x+ch),x,xt1,xt2)
                s1 = (xt2-xt1)*((1./2.)*ah*(xt2+xt1)+ch)            
                ! s2=int((ch)*(-exp(ae*x+ce)),x,xt1,xt2)
                ceae=ce/ae;
                s2 = -ch*exp(ae*(xt1+ceae))*(xt2-xt1)*intexp(ae*(xt2-xt1))                
                ! s3=int((ah*x)*(-exp(ae*x+ce)),x,xt1,xt2)
                ! s3=int((ah*x)*(-exp(ae*(x+ceae))),x,xt1,xt2)
                ! expand in Taylor series around ae=0
                ! collect(expand(taylor(int(x*(-exp(ae*(x+ceae))),x,xt1,xt2)*ae^2,ae,4)/ae^2),ae)
                ! =(1/8*xt1^4+1/3*xt1^3*ceae+1/4*xt1^2*ceae^2-1/8*xt2^4-1/3*xt2^3*ceae-1/4*xt2^2*ceae^2)*ae^2
                !     + (-1/3*xt2^3-1/2*xt2^2*ceae+1/3*xt1^3+1/2*xt1^2*ceae)*ae 
                !     + 1/2*xt1^2-1/2*xt2^2
                !
                ! coefficient at ae^2 in the expansion, after some algebra            
                a2=(xt1-xt2)*((1./4.)*(xt1+xt2)*ceae**2+(1./3.)* &
                   (xt1**2+xt1*xt2+xt2**2)*ceae+(1./8.)* &
                   (xt1**3+xt1*(xt2**2)+xt1**2*xt2+xt2**3))               
                d=(ae**4)*a2
            
                if (abs(d)>eps) then
                ! since ae*xt1+ce<=0 ae*xt2+ce<=0 all fine for large ae
                ! for ae, ce -> 0 rounding error approx eps/ae^2
                s3=( exp(ae*(xt1+ceae))*(ae*xt1-1)-&
                     exp(ae*(xt2+ceae))*(ae*xt2-1) )/(ae**2)
                !we do not worry about rounding as xt1 -> xt2, then s3 -> 0
                else
                ! coefficient at ae^1 in the expansion
                a1=(xt1-xt2)*((1./2.)*ceae*(xt1+xt2)+(1./3.)*&
                   (xt1**2+xt1*xt2+xt2**2))
                ! coefficient at ae^0 in the expansion for ae->0
                a0=(1./2.)*(xt1-xt2)*(xt1+xt2)
                s3=a0+a1*ae+a2*ae**2; ! approximate the integral
                end if
                s3=ah*s3                                                
                fuel_burn(i,j)=fuel_burn(i,j)+s1+s2+s3
                if(fuel_burn(i,j)<0 .or. fuel_burn(i,j)>dx*dy) then                
                  print *,'fuel_fraction should be between 0 and 1'
                end if
              end if
            end do ! k     
            fuel_burn(i,j)=fuel_burn(i,j)/(dx*dy)
        end if ! if case0, elseif case4 ,else case123
    
    end do !j
end do ! i
            
end subroutine ! fuel_burnt                  
!
!****************************************
!
subroutine sortxt(xytlist,nrow,ncolumn,xt,nxt,nvec)
implicit none
integer::nrow,ncolumn,nxt,nvec
real,dimension(nrow,ncolumn)::xytlist
real,dimension(nxt)::xt

integer::i,j
real::temp

do i=1,nvec
  xt(i)=xytlist(i,1)
end do

do i=1,nvec-1
  do j=i+1,nvec
    if ( xt(i) > xt(j) ) then
      temp = xt(i)
      xt(i)=xt(j)
      xt(j)=temp
    end if
  end do
end do

end subroutine !sortxt
!
!****************************************
!
real function intexp(ab)
implicit none
real::ab
!calls
intrinsic epsilon

real, parameter:: zero=0.,one=1.,eps=epsilon(zero)

!eps = 2.2204*(10.0**(-8))!from matlab
if ( eps < abs(ab)**3/6. ) then
    intexp=(exp(ab)-1)/ab
else
    intexp=1+ab/2.
end if
end function
!
!****************************************
!
subroutine matvec(A,m,n,V,nv,out,nout,nrow,ncolumn)
implicit none
integer::m,n,nv,nout,nrow,ncolumn
real,dimension(m,n)::A   ! allocated m by n 
real,dimension(nv)::V    ! allocated nv
real,dimension(nout)::out! allocated nout 

integer::i,j

do i=1,nrow
  out(i)=0.0
  do j=1,ncolumn
    out(i)=out(i)+A(i,j)*V(j)
  end do
end do
end subroutine
!
!****************************************
!
subroutine matmatp(A,mA,nA,B,mB,nB,C,mC,nC,nrow,ncolumn,nP)
implicit none
integer::mA,nA,mB,nB,mC,nC,nrow,ncolumn,nP
real,dimension(mA,nA)::A   ! allocated m by n 
real,dimension(mB,nB)::B   ! allocated m by n 
real,dimension(mC,nC)::C   ! allocated m by n 

integer::i,j,k

do i=1,nrow  
  do j=1,ncolumn
    C(i,j)=0.0
    do k=1,nP
      C(i,j)=C(i,j)+A(i,k)*B(j,k) ! B'
    end do
  end do
end do
end subroutine
!
!****************************************
!
subroutine prop_ls(ids,ide,jds,jde, &                       ! domain dims
                   ims,ime,jms,jme, &                       ! memory dims
                   num_tiles,i_start,i_end,j_start,j_end, & ! tiling
                   ts,td,dx,dy,     &                       ! scalars in
                   lfn,tc           &                       ! arrays inout                   
#ifdef SPEED_VARS_ARGS      /* extra arguments for normal_spread */
#include SPEED_VARS_ARGS
#else
#include "fr_sfire_params_args.h"
#endif
                   )

implicit none

!*** purpose: advance level function in time

! Jan Mandel and Minjeong Kim August 2007

!*** description
!
! Propagation of closed curve by a level function method. The level function
! lfn is defined by its values at the nodes of a rectangular grid. 
! The area where lfn < 0 is inside the curve. The curve is 
! described implicitly by lfn=0. Points where the curve intersects gridlines
! can be found by linear interpolation from nodes.
!
! The level function is advanced from time ts to time ts + dt. 
!
! The level function should be initialized to (an approximation of) the signed
! distance from the curve. If the initial curve is a circle, the initial level
! function is simply the distance from the center minus the radius.
! 
! The curve moves outside with speed given by function normal_spread.
! 
! ------------------------------------
! Based on S. Osher and R. Fedkiw, Level set methods and dynamic implicit surfaces,
! Springer, 2003, Sec. 6.4, as implemented in toolboxLS for Matlab by 
! I. Mitchell, A toolbox of Level Set Methods (Version 1.1), TR-2007-11,
! Dept. Computer Science, University of British Columbia, 2007
! http://www.cs.ubc.ca/\~mitchell/Toolbo\LS
!   
! Method: Godunov method for the normal motion. The timestep is set automatically based on
! CFL condition. For a straight segment in a constant field and locally linear
! level function, the method reduces to the exact normal motion. The advantage of 
! the level set method is that it treats automatically special cases such as
! the curve approaching itself and merging components of the area inside the curve.
!
  
!*** arguments 

! ims,ime,jms,jme   in    array dimensions
! ids,ide,jds,jde   in    grid dimensions in the array, in cells (in nodes, +1)
! ts                in    start time
! td                in    end time is ts+td (given this way because of rounding)
! dx,dy             in    grid spacing
! lfn               inout the level function
! tc                inout the time the contour=zero level set crossed the node

!*** calls
!
! tend_ls
!

! for essentially the same result as using advection vx, vy, and 
! spread rate r directly, use instead of call normal_spread:
!   speed = sr + max(vx*nvx + vy*nvy,0.0)
!      
integer,intent(in)::ims,ime,jms,jme,ids,ide,jds,jde
integer,intent(in)::num_tiles
integer,intent(in),dimension(num_tiles)::i_start,i_end,j_start,j_end
real,dimension(ims:ime,jms:jme),intent(inout)::lfn,tc
real,intent(in)::dx,dy,ts,td
#ifdef SPEED_VARS_DECL      /* extra arguments for normal_spread */
#include SPEED_VARS_DECL
#else
#include "fr_sfire_params_decl.h"
#endif

!*** local 
! arrays
real,dimension(ims:ime,jms:jme):: & ! same mesh as lfn
   tend, speed, lfn0, lfn1
! scalars
real::t,dt,tr,tbound,grad2,a
real::gradx,grady,aspeed,err,aerr
integer::istep
integer::i,j
character(len=128)msg
logical last
integer::nfirenodes,nfireline
real::sum_err,min_err,max_err,sum_aerr,min_aerr,max_aerr    
#ifdef DEBUG_OUT    
integer::id
#endif


! constants
integer,parameter :: mstep=1000, printl=1
real, parameter:: zero=0.,one=1.,eps=epsilon(zero),tol=100*eps, &
    safe=2.,rmin=safe*tiny(zero),rmax=huge(zero)/safe

! f90 intrinsic function

intrinsic max,min,sqrt,nint,epsilon,tiny,huge
  
!*** executable

call check_mesh_2dim(ids,ide+1,jds,jde+1,ims,ime,jms,jme)
call check_tiles(ids,ide,jds,jde,num_tiles,i_start,i_end,j_start,j_end)

write(msg,'(a,i5,a,i5,a,f6.2,a,f6.2)')'prop_ls: mesh ',ide-ids+1,' by',jde-jds+1,&
    ' spacing',dx,' by',dy
call message(msg)

if (td < 0) then
    write(msg,*)'prop_ls: time advance td=',td,' must be non-negative' 
    call crash(msg)
endif    
if (.not. td > 0) return ! td=0, nothing to do

! initialize
tr=0       ! relative time offset from ts
istep = 0  ! step number - just to prevent an infinite loop, should never be reached
last = .false. ! stopping test

do while ( .not.last )
    istep=istep+1

#ifdef DEBUG_OUT    
    id=istep*10
    if(istep.eq.1)call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,lfn,'lfn',id)
#endif

    ! time at the beginning of the timestep
    t = ts + tr  ! advance tr instead of t because of rounding
    
    
    ! tend = F(lfn)

    call  tend_ls(ids,ide,jds,jde, &                       ! domain dims
                   ims,ime,jms,jme, &                       ! memory dims
                   num_tiles,i_start,i_end,j_start,j_end, & ! tiling
                   id+1,t,dx,dy,        &                       ! scalars in
                   lfn, &                                   ! arrays in
                   tbound, &                                ! scalars out 
                   tend,speed &                             ! arrays out
#ifdef SPEED_VARS_ARGS      /* extra arguments for normal_spread */
#include SPEED_VARS_ARGS
#else
#include "fr_sfire_params_args.h"
#endif
)

    dt = min(td-tr, 0.5*tbound)

    ! lfn1 = lfn+dt*tend
    ! lfn0 = lfn
    
    do j=jds,jde+1
        do i=ids,ide+1
            lfn0(i,j) = lfn(i,j)
            lfn1(i,j) = lfn(i,j) + dt*tend(i,j)
        enddo
    enddo
    
    ! tend = F(lfn1)

    call  tend_ls(ids,ide,jds,jde, &                       ! domain dims
                   ims,ime,jms,jme, &                       ! memory dims
                   num_tiles,i_start,i_end,j_start,j_end, & ! tiling
                   id+2,t,dx,dy,        &                       ! scalars in
                   lfn1, &                                   ! arrays in
                   tbound, &                                ! scalars out 
                   tend,speed &                             ! arrays out
#ifdef SPEED_VARS_ARGS      /* extra arguments for normal_spread */
#include SPEED_VARS_ARGS
#else
#include "fr_sfire_params_args.h"
#endif
)

    ! combine lfn1 and lfn0 + dt*tend -> lfn
    
    a=0.5    ! 0 = euler, 1/2 = heun
    do j=jds,jde+1
        do i=ids,ide+1
            lfn(i,j) = (1.-a)* lfn1(i,j) + a*(lfn0(i,j) + dt*tend(i,j))
            ! lfn(i,j) = min( lfn1(i,j) , lfn0(i,j) + dt*tend(i,j) )
        enddo
    enddo      
    
    ! compute ignition time by interpolation
    ! the node was not burning at start but it is burning at end
    ! interpolate from the level functions at start and at end
    ! lfn0 is the level function value at time t
    ! lfn  is the level function value at time t+dt
    ! 0       should the level function value at time tc(i,j)
    ! thus assuming the level function is approximately linear =>
    ! tc(i,j)= t + ((t + td) - t) * lfn0 / (lfn0 - lfn)
    !        = t + dt * lfn0 / (lfn0 - lfn)
        

    do j=jds,jde+1
        do i=ids,ide+1
            ! interpolate the cross-over time
            if (.not. lfn(i,j)>0 .and. lfn0(i,j)>0)then
                tc(i,j) = t - dt * lfn0(i,j) / (lfn0(i,j) - lfn(i,j))
            endif
        enddo
    enddo
    
    ! check local speed error and stats 
    ! init stats
    nfirenodes=0
    nfireline=0
    sum_err=0.
    min_err=rmax
    max_err=rmin     
    sum_aerr=0.
    min_aerr=rmax
    max_aerr=rmin    
    ! loop over right outside of the domain
    do j=jds+1,jde+1-1
        do i=ids+1,ide+1-1
            if(lfn(i,j)>0.0)then                      ! a point out of burning region
                if(lfn(i+1,j)<=0.or.lfn(i,j+1)<=0.or. & ! neighbor in burning region
                   lfn(i-1,j)<=0.or.lfn(i,j-1)<=0)then ! point next to fireline
                   gradx=(lfn(i+1,j)-lfn(i-1,j))/(2.0*dx) ! central differences
                   grady=(lfn(i,j+1)-lfn(i,j-1))/(2.0*dy)
                   grad2=sqrt(gradx*gradx+grady*grady)
                   aspeed = (lfn0(i,j)-lfn(i,j))/(dt*max(grad2,rmin))
                   err=aspeed-speed(i,j)
                   sum_err=sum_err+err
                   min_err=min(min_err,err)
                   max_err=max(max_err,err)     
                   aerr=abs(err)
                   sum_aerr=sum_aerr+aerr
                   min_aerr=min(min_aerr,aerr)
                   max_aerr=max(max_aerr,aerr)
                   nfireline=nfireline+1
                endif
            else
                nfirenodes=nfirenodes+1
            endif
        enddo
    enddo
    write(msg,'(2(a,i6,f8.4))')'prop_ls: nodes burning',nfirenodes, &
        (100.*nfirenodes)/((ide+1-ids)*(jde+1-jds)),'% next to fireline',nfireline
    call message(msg)
    if(nfireline>0)then
        call print_stat_line('speed error',min_err,max_err,sum_err/nfireline)
        call print_stat_line('abs(speed error)',min_aerr,max_aerr,sum_aerr/nfireline)
    endif
        
    tr = tr + dt
    last = .not.(tr + td*tol < td .and. istep < mstep)   !  last pass

    if(printl>=2 .or. (printl>=1 .and. (last .or. istep==1) ) )then
        write(msg,'(7(a,f12.2))')  &
            'prop_ls:time',t,' dt',dt
        call message(msg)
        call print_2d_stats(ids,ide+1,jds,jde+1,ims,ime,jms,jme,tend,'prop_ls:lvl fcn tend(1/s)')
    endif
    
end do !while

if (printl>=1)then
    write(msg,'(a,i5)')'prop_ls: total number of time steps ',istep
    call message(msg)
endif
    
if (abs(tr - td) > 2*tol .or. istep >= mstep) then
    write(msg,*)'prop_ls: time advanced by ',tr,' of ',td
    call message(msg)
    write(msg,*)'diff ',tr-td, ' in ',istep,' steps of ',mstep
    call message(msg)    
    call crash('prop_ls: end time not reached or max steps exceeded')
endif

end subroutine prop_ls

!
!*****************************
!

subroutine tend_ls(ids,ide,jds,jde, &                       ! domain dims
                   ims,ime,jms,jme, &                       ! memory dims
                   num_tiles,i_start,i_end,j_start,j_end, & ! tiling
                   id,t,dx,dy,      &                       ! scalars in
                   lfn, &                                   ! arrays in
                   tbound, &                                ! scalars out 
                   tend, speed  &                           ! arrays out
#ifdef SPEED_VARS_ARGS      /* extra arguments for normal_spread */
#include SPEED_VARS_ARGS
#else
#include "fr_sfire_params_args.h"
#endif
)

use module_fr_sfire_phys
implicit none

!*** calls
!
! normal_spread
!

! for essentially the same result as using advection vx, vy, and 
! spread rate r directly, use instead of call normal_spread:
!   speed = sr + max(vx*nvx + vy*nvy,0.0)
!      
integer,intent(in)::ims,ime,jms,jme,ids,ide,jds,jde
integer,intent(in)::num_tiles
integer,intent(in),dimension(num_tiles)::i_start,i_end,j_start,j_end
integer,intent(in)::id                                 ! for debug dumps
real,intent(in)::t                                     ! time
real,intent(in)::dx,dy                                 ! mesh step
real,dimension(ims:ime,jms:jme),intent(in)::lfn        ! level function
real,intent(out)::tbound                               ! max allowed time step
real,dimension(ims:ime,jms:jme),intent(out)::tend      ! tendency (rhs of the diff eq)
real,dimension(ims:ime,jms:jme),intent(out)::speed     ! normal propagation (testing only)
#ifdef SPEED_VARS_DECL      /* extra arguments for normal_spread */
#include SPEED_VARS_DECL
#else
#include "fr_sfire_params_decl.h"
#endif

!*** local 
! arrays
real,dimension(ims:ime,jms:jme):: & ! same mesh as lfn
   diffLx,diffLy,diffRx,diffRy, & 
   diffCx,diffCy,nvx,nvy,scale,diff2x,diff2y,grad
real, dimension((jde-jds+2)*(ide-ids+2))::rii,rjj,speedp
! scalars
real::rr
integer::i,j,k,kk
!character(len=128)msg

! constants
real, parameter:: zero=0.,one=1.,eps=epsilon(zero),tol=100*eps, &
    safe=2.,rmin=safe*tiny(zero),rmax=huge(zero)/safe

! f90 intrinsic function

intrinsic max,min,sqrt,nint,epsilon,tiny,huge
  
!*** executable

! prepare the index list for the speed function
! in this version, we will want speed at all mesh nodes.
! in general only a subset may be asked

kk=0
do j=jds,jde+1
    do i=ids,ide+1
         kk=kk+1
         rii(kk)=i
         rjj(kk)=j
    enddo
enddo
k=kk    


    ! one sided differences
    ! we waste a little and store them separately to make the code more
    ! readable, and to allow for higher order scheme in future if needed
    ! allow for general array bounds n1:n,m1:m - may be useful in parallel
    
    call meshdiff_2d(        &           ! get all 4 one-sided differences
        ids,ide,  jds,jde ,  &           ! mesh area used (in cells, end +1)
        ims,ime,  jms,jme,   &           ! memory dimensions  
        dx,dy,               &           ! mesh spacing
        lfn,                 &           ! input
        diffLx,diffRx,diffLy,diffRy) ! output        

    do j=jds,jde+1
        do i=ids,ide+1
        
            ! 2 times central differences
            diffCx(i,j)=diffLx(i,j) + diffRx(i,j)
            diffCy(i,j)=diffLy(i,j) + diffRy(i,j)
    
            ! Godunov scheme: upwind differences, L or R or none    
            ! always test on > or < never = , much faster because of IEEE
     
            ! central diff >= 0 => take left diff if >=0, ortherwise 0
            ! central diff <= 0 => take right diff if <=0, ortherwise 0
            diff2x(i,j)=0
            diff2y(i,j)=0
            if (.not.diffLx(i,j)<0.and..not.diffCx(i,j)<0)diff2x(i,j)=diff2x(i,j)+diffLx(i,j)
            if (.not.diffRx(i,j)>0.and.     diffCx(i,j)<0)diff2x(i,j)=diff2x(i,j)+diffRx(i,j)
            if (.not.diffLy(i,j)<0.and..not.diffCy(i,j)<0)diff2y(i,j)=diff2y(i,j)+diffLy(i,j)
            if (.not.diffRy(i,j)>0.and.     diffCy(i,j)<0)diff2y(i,j)=diff2y(i,j)+diffRy(i,j)
!            if (diffLx(i,j)>0.and.diffCx(i,j)>0)diff2x(i,j)=diffLx(i,j)
!            if (diffRx(i,j)<0.and.diffCx(i,j)<0)diff2x(i,j)=diffRx(i,j)
!            if (diffLy(i,j)>0.and.diffCy(i,j)>0)diff2y(i,j)=diffLy(i,j)
!            if (diffRy(i,j)<0.and.diffCy(i,j)<0)diff2y(i,j)=diffRy(i,j)
            
            ! magnitude of the gradient
            grad(i,j)=sqrt(diff2x(i,j)*diff2x(i,j) + diff2y(i,j)*diff2y(i,j))

            ! normal direction
            scale(i,j)=sqrt(diffCx(i,j)*diffCx(i,j)+diffCy(i,j)*diffCy(i,j)+eps) 
            nvx(i,j)=diffCx(i,j)/scale(i,j);
            nvy(i,j)=diffCy(i,j)/scale(i,j);
            
!            scale(i,j)=grad(i,j)+rmin 
!            nvx(i,j)=diff2x(i,j)/scale(i,j);
!            nvy(i,j)=diff2y(i,j)/scale(i,j);

        enddo
    enddo
    
    ! get propagation speed
    call normal_spread(t,dx,dy,k,rii,rjj,ids,ide,jds,jde,ims,ime,jms,jme,nvx,nvy,scale,speedp &
#ifdef SPEED_VARS_ARGS  /* extra arguments for the speed functions*/
#include SPEED_VARS_ARGS
#else
#include "fr_sfire_params_args.h"
#endif    
    ) 
    do kk=1,k
       i=nint(rii(kk))
       j=nint(rjj(kk))
       speed(i,j)=speedp(kk)
    enddo
    call print_2d_stats(ids,ide,jds,jde,ims,ime,jms,jme,speed,'prop_ls:in: frln speed (m/s)')

    tbound=0
    do j=jds,jde+1
        do i=ids,ide+1
            rr=speed(i,j)
                
            ! time step bound - CFL condition
            ! contribution of the normal term
            if (grad(i,j) > 0.) then
                tbound = max(tbound,rr*(abs(diff2x(i,j))/dx+abs(diff2y(i,j))/dy)/grad(i,j))
            endif
            
            ! the rhs of the diff eq, a.k.a. lfn dot, a.k.a. the lfn "tendency"
            tend(i,j) = -rr*grad(i,j)   ! normal term rhs
        enddo
    enddo        

    ! the final CFL bound
    tbound = 1/(tbound+tol)
    

#ifdef DEBUG_OUT
    if(mod(id,50).eq.0)then
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,speed,'speed',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,diff2x,'diff2x',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,diff2y,'diff2y',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,diff2x,'diffCx',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,diff2y,'diffCy',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,nvx,'nvx',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,nvy,'nvy',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,vx,'vx',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,vy,'vy',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,tend,'tend',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,lfn,'lfn',id)
    endif
#endif

end subroutine tend_ls

end module module_fr_sfire_core
