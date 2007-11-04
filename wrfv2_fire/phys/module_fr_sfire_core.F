!
!*** Jan Mandel August-October 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
! With contributions by Minjeong Kim.

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
real:: time_now,avg_rate,max_rate
integer::i,j
character(len=128)msg

!*** calls
intrinsic epsilon


!*** executable

call check_mesh_2dim(ids,ide+1,jds,jde+1,ims,ime,jms,jme)
call check_tiles(ids,ide,jds,jde,num_tiles,i_start,i_end,j_start,j_end)

! compute the fuel fraction at time_start - not needed if passed in
call fuel_left(ids,ide,jds,jde,ims,ime,jms,jme, &
    lfn,tign,fuel_time,time_start,frac_start)

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
call fuel_left(ids,ide,jds,jde,ims,ime,jms,jme, &
    lfn,tign,fuel_time,time_start+time_diff,fuel_frac)

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
! 1. The output should be a continuous function of the arrays lfn and tign whenever 
!    lfn(i,j)=0 implies tign(i,j)=tnow.  
! 2. The output should be invariant to the symmetries of the input in each cell.
! 3. Arbitrary combinations of the signs of lfn(i,j) should work.
! 4. The result should be at least 1st order accurate in the sense that it is exact if 
!    the time from ignition is a linear function.
!
! Note: If time from ignition is approximated by polynomial in the burnt
! region of the cell, this is integral of polynomial times exponential
! over a polygon, which can be computed exactly.
!
! Requirement 4 is particularly important when there is a significant decrease
! of the fuel fraction behind the fireline on the mesh scale, because the rate of fuel
! decrease right behind the fireline is much larger (exponential...). This will happen when
! 
!      change of time from ignition withing one mesh cell
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
! When X is large then the fuel burnt then in one timestep in the cell is approximately
! proportional to length of  fireline in that cell.
! When X is small then the fuel burnt then in one timestep in the cell is approximately
! proportional to the area of the burning region.
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

use module_fr_sfire_phys
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
! subroutine normal_spread(t,n,rii,rjj,ims,ime,jms,jme,nvx,nvy,scale,speed)
! t               in time
! n               in number of points to evaluate the speed at
! rii,rjj         in their grid coordinates i,j (may not be integer)
! ims,ime,jms,jme in array dimensions (in cells)
! nvx,nvy         in normal direction (gradient of level function)
! scale           in norm of gradient of level function
! speed           out the spread rate in the normal directin
!
! Note
! t may be used to interpolate data in time.
! when scale is small then normal direction is not meaningful

! for essentially the same result as using advection vx, vy, and 
! spread rate r directly, use instead of call normal_spread:
!   speed = sr + max(vx*nvx + vy*nvy,0.0)
      
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
   tend,diffLx,diffLy,diffRx,diffRy, & 
   diffCx,diffCy,nvx, nvy,speed, scale
#ifdef DEBUG_OUT
real,dimension(ims:ime,jms:jme)::    vvxx,vvyy,aa,rrr
#endif
real, dimension((jde-jds+2)*(ide-ids+2))::rii,rjj,speedp
! scalars
real:: diff2x,diff2y,rr,grad,t,dt,tr,tbound,lfn_old,lfn_new
integer::istep
integer:: i,j,k,kk
character(len=128)msg
logical last

! constants
integer,parameter :: mstep=1000, printl=1
real, parameter:: zero=0.,one=1.,eps=epsilon(zero),tol=100*eps

! f90 intrinsic function

intrinsic max,min,sqrt,nint,epsilon
  
!*** executable

call check_mesh_2dim(ids,ide+1,jds,jde+1,ims,ime,jms,jme)
call check_tiles(ids,ide,jds,jde,num_tiles,i_start,i_end,j_start,j_end)

if (td < 0) then
    write(msg,*)'prop_ls: time advance td=',td,' must be non-negative' 
    call crash(msg)
endif    
if (.not. td > 0) return ! td=0, nothing to do

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

! initialize
tr=0       ! relative time offset from ts
istep = 0  ! step number - just to prevent an infinite loop, should never be reached
last = .false. ! stopping test

do while ( .not.last )
    istep=istep+1

#ifdef DEBUG_OUT    
        if(istep.eq.1)call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,lfn,'lfn',id)
#endif

    t = ts + tr  ! advance tr instead of t because of rounding

    ! one sided differences
    ! we waste a little and store them separately to make the code more
    ! readable, and to allow for higher order scheme in future if needed
    ! allow for general array bounds n1:n,m1:m - may be useful in parallel
    
    call meshdiff_2d(        &           ! get all 4 one-sided differences
        ids,ide,  jds,jde ,  &           ! mesh area used (in cells, end +1)
        ims,ime,  jms,jme,   &           ! dimensions of lfn 
        ims,ime,  jms,jme,   &           ! dimensions of the outputs
        dx,dy,               &           ! mesh spacing
        lfn,                 &           ! input
        diffLx,diffRx,diffLy,diffRy) ! output        
    !print *,'err=', &
    !    sum(abs(diffLx2-diffLx))+sum(abs(diffRx2-diffRx)) &
    !   +sum(abs(diffLy2-diffLy))+sum(abs(diffRy2-diffRy)) 

    do j=jds,jde+1
        do i=ids,ide+1
        
            ! 2 times central differences
            diffCx(i,j)=diffLx(i,j) + diffRx(i,j)
            diffCy(i,j)=diffLy(i,j) + diffRy(i,j)
    
            ! normal direction
            scale(i,j)=sqrt(diffCx(i,j)*diffCx(i,j)+diffCy(i,j)*diffCy(i,j)+eps) 
            nvx(i,j)=diffCx(i,j)/scale(i,j);
            nvy(i,j)=diffCy(i,j)/scale(i,j);
            
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

#ifdef DEBUG_OUT
            if(istep.eq.1)call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,speed,'speed',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,nvx,'nvx',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,nvy,'nvy',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,vx,'vx',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,vy,'vy',id)
#endif
    tbound=0
    do j=jds,jde+1
        do i=ids,ide+1
            rr=max(speed(i,j),0.)  ! just making sure
            
            ! Godunov scheme: choose the upwind direction, L or R or none    
            ! always test on > or < never = , much faster because of IEEE
     
            diff2x=0
            diff2y=0
            if (.not.diffLx(i,j)<0.and..not.diffCx(i,j)<0)diff2x=diff2x+diffLx(i,j)
            if (.not.diffRx(i,j)>0.and.     diffCx(i,j)<0)diff2x=diff2x+diffRx(i,j)
            if (.not.diffLy(i,j)<0.and..not.diffCy(i,j)<0)diff2y=diff2y+diffLy(i,j)
            if (.not.diffRy(i,j)>0.and.     diffCy(i,j)<0)diff2y=diff2y+diffRy(i,j)
    
            ! magnitude of the gradient
            grad=sqrt(diff2x*diff2x + diff2y*diff2y)
    
            ! time step bound - CFL condition
            ! contribution of the normal term
            if (grad > 0.) then
                tbound = max(tbound,rr*(abs(diff2x)/dx+abs(diff2y)/dy)/grad)
            endif
            
            ! tendencies
            tend(i,j) = -rr*grad   ! normal term rhs
        enddo
    enddo        

#ifdef DEBUG_OUT
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,tend,'tend',id)
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,vvxx,'vvxx',id)
    
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,aa,'aa',id)
    
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,rrr,'rrr',id)
    
            call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,vvyy,'vvyy',id)
#endif
         
    ! the final CFL bound and the timestep
    tbound = 1/(tbound+tol)
    dt = min(td-tr, 0.5*tbound)
    
    ! the rhs of the diff eq, a.k.a. lfn dot, a.k.a. the lfn "tendency"
    ! 0. because max requires both arguments same type (real) 
    
    ! compute ignition time by interpolation
    ! the node was not burning at start but it is burning at end
    ! interpolate from the level functions at start and at end
    ! lfn_old is the level function value at time t
    ! lfn_new is the level function value at time t+dt
    ! 0       should the level function value at time tc(i,j)
    ! thus assuming the level function is approximately linear =>
    ! tc(i,j)=t + ((t + td) - t) * lfn_old / (lfn_old - lfn_new)
        
    do j=jds,jde+1
        do i=ids,ide+1
            lfn_old = lfn(i,j)
            lfn_new = lfn(i,j) + dt*tend(i,j) 
            ! interpolate the cross-over time
            if(lfn_old>0 .and. .not. lfn_new>0)then
                tc(i,j) = t - lfn_old / tend(i,j)
            endif
            ! advance one step
            lfn(i,j) = lfn_new
        enddo
    enddo

#ifdef DEBUG_OUT
            if(istep.eq.1)call write_array_m(ids,ide+1,jds,jde+1,           &
                                ims,ime,jms,jme,lfn,'lfn_new',id)
#endif
    
    tr = tr + dt
    last = .not.(tr + td*tol < td .and. istep < mstep)   !  last pass

    if(printl>=2 .or. (printl>=1 .and. (last .or. istep==1) ) )then
        write(msg,'(7(a,f10.5))')  &
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

end module module_fr_sfire_core
