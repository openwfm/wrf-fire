!
!*** Jan Mandel August-October 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
! With contributions by Minjeong Kim.
!#define DEBUG_OUT

module module_fr_sfire_core

use module_fr_sfire_phys
use module_fr_sfire_util

! The mathematical core of the fire spread model. No physical constants here.
! 
! subroutine sfire_core: only this routine should be called from the outside.
! subroutine fuel_left:  compute remaining fuel from time of ignition.
! subroutine prop_ls: propagation of curve in normal direction.

contains

    
subroutine  get_fuel_left(                  &
    ims,ime,jms,jme,                        &
    its,ite,jts,jte,                        &
    ifs,ife,jfs,jfe,                        &
    fire_dx,fire_dy,                        &
    lfn,tign,fuel_time,time_now,fuel_frac)
implicit none
!*** purpose: wrapper for fuel_left

!*** arguments     
integer, intent(in) ::ims,ime,jms,jme,its,ite,jts,jte,ifs,ife,jfs,jfe
real, intent(in):: fire_dx,fire_dy,time_now
real, intent(in), dimension(ims:ime,jms:jme)::lfn,tign,fuel_time
real, intent(out), dimension(ifs:ife,jfs:jfe)::fuel_frac

!*** local
integer:: fuel_left_type

!*** executable

fuel_left_type=1
if (fuel_left_type .eq. 1)then
    ! somewhat improved but still bumpy
    call fuel_left_jm( &
        ims,ime,jms,jme, &
        its,ite,jts,jte, &
        ifs,ife,jfs,jfe, &
        lfn,tign,fuel_time,time_now,fuel_frac)
else
        ! the new version comes here
endif
end subroutine get_fuel_left

!
!****************************************
!
    
subroutine init_no_fire(&
    ifds,ifde,jfds,jfde, &
    ifms,ifme,jfms,jfme, &
    ifts,ifte,jfts,jfte, &
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
 
do j=jfts,enode(jfte,jfde)
    do i=ifts,enode(ifte,ifde)
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
                        ifms,ifme,jfms,jfme,                      &
                        ifts,ifte,jfts,jfte,                      &
                        sx,sy,ex,ey,r,time_ign,fdx,fdy,           &
                        lfn,tign,ignited)
implicit none

!*** purpose: ignite a circular/line fire 

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
integer:: i,j,ifte1,jfte1
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
jfte1=jfte
if(jfte.eq.jfde)jfte1=jfte1+1
ifte1=ifte
if(ifte.eq.ifde)ifte1=ifte1+1
do j=jfts,jfte1      ! node based loops, hence the +1
    do i=ifts,ifte1
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
write(msg,'(a,2f10.1,a,2f10.1,a,f8.1,a,f8.1,a,i6)')'ignite_fire: from',sx,sy,' to ',&
    ex,ey,' radius ',r,' time',time_ign,' ignited nodes',ignited
call message(msg)
end subroutine ignite_fire

!
!**********************
!            

subroutine fuel_left_jm( &
    ims,ime,jms,jme, &
    its,ite,jts,jte, &
    ifs,ife,jfs,jfe, &
    lfn, tign, fuel_time, tnow, fuel_frac)
implicit none

!*** purpose: determine fraction of fuel remaining

!*** Jan Mandel August 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com

!*** arguments

integer, intent(in) :: its,ite,jts,jte,ims,ime,jms,jme,ifs,ife,jfs,jfe
real, intent(in), dimension(ims:ime,jms:jme)::lfn,tign,fuel_time
real, intent(in):: tnow
real, intent(out), dimension(ifs:ife,jfs:jfe)::fuel_frac

! ims,ime,jms,jme   in   memory dimensions                (1)
! its,ite,jts,jte   in   tile dimensions (cells)          (1)
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
!
!                                  /\
!                         1        |              T(x,y)-tnow
! fuel_frac(i,j)  =  1 - -----     | 1 -  exp( - ------------- ) dxdy
!                        dx*dy     |             fuel_time(i,j)
!                                 \/
!                           dx*(i-1)<x<dx*i
!                           dy*(j-1)<y<dy*j
!                             lfn(x,y)<=0
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

call check_mesh_2dim(its,ite+1,jts,jte+1,ims,ime,jms,jme)

! precompute nodal quantities
do j=jts,jte+1     ! note the order of indices for fast  execution
    do i=its,ite+1  
        ap(i,j)=abs(lfn(i,j))
        ! scaled time since ignition         
        if (lfn(i,j)>0) then
            t(i,j)=0.
        else
            t(i,j)=tnow - tign(i,j)
        endif
    enddo
enddo

do j=jts,jte
    do i=its,ite ! it is OK to introduce extra scalars, just as fast

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

subroutine prop_ls( id, &                                ! for debug
                ids,ide,jds,jde, &                       ! domain dims
                ims,ime,jms,jme, &                       ! memory dims
                its,ite,jts,jte, &                       ! tile dims
                ts,dt,dx,dy,     &                       ! scalars in
                tbound,          &                       ! scalars out
                lfn_in,lfn_out,tign         &                     ! arrays inout                   
#ifdef SPEED_VARS_ARGS      /* extra arguments for normal_spread */
#include SPEED_VARS_ARGS
#else
#include "fr_sfire_params_args.h"
#endif
                   )
implicit none

!*** purpose: advance level function in time

! Jan Mandel August 2007 - February 2008

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
! Method: Godunov method for the normal motion. The timestep is checked for
! CFL condition. For a straight segment in a constant field and locally linear
! level function, the method reduces to the exact normal motion. The advantage of 
! the level set method is that it treats automatically special cases such as
! the curve approaching itself and merging components of the area inside the curve.
!
! Based on S. Osher and R. Fedkiw, Level set methods and dynamic implicit surfaces,
! Springer, 2003, Sec. 6.4, as implemented in toolboxLS for Matlab by 
! I. Mitchell, A toolbox of Level Set Methods (Version 1.1), TR-2007-11,
! Dept. Computer Science, University of British Columbia, 2007
! http://www.cs.ubc.ca/\~mitchell/Toolbo\LS
! 
  
!*** arguments 

! id                in    unique identification for prints and dumps
! ids,ide,jds,jde   in    domain dimensions
! ims,ime,jms,jme   in    memory dimensions
! its,ite,jts,jte   in    tile dimensions
! ts                in    start time
! dt                in    time step
! dx,dy             in    grid spacing
! lfn_in,lfn_out    inout,out the level set function at nodes
! tign              inout the ignition time at nodes

! The dimensions are cell-based, the nodal value is associated with the south-west corner.
! The whole computation is on domain indices ids:ide+1,jds:jde+1.
!
! The region where new lfn and tign are computed is the tile its:ite,jts:jte 
! except when the tile is at domain upper boundary, an extra band of points is added:
! if ite=ide then region goes up to ite+1, if jte=jde then region goes up to jte+1.

! The time step requires values from 2 rows of nodes beyond the region except when at the 
! domain boundary one-sided derivatives are used. This is implemented by extending the input
! beyond the domain boundary so sufficient memory bounds must be allocated. 
! The update on all tiles can be done in parallel. To avoid the race condition (different regions
! of the same array updated by different threads), the in and out versions of the
! arrays lft and tign are distinct. If the time step dt is larger
! that the returned tbound, the routine should be called again with timestep td<=tbound, and then
! having distinct inputs and outputs comes handy.

!*** calls
!
! tend_ls
!

integer,intent(in)::id,ims,ime,jms,jme,ids,ide,jds,jde,its,ite,jts,jte
real,dimension(ims:ime,jms:jme),intent(inout)::lfn_in,tign
real,dimension(ims:ime,jms:jme),intent(out)::lfn_out
real,intent(in)::dx,dy,ts,dt
real,intent(out)::tbound
#ifdef SPEED_VARS_DECL      /* extra arguments for normal_spread */
#include SPEED_VARS_DECL
#else
#include "fr_sfire_params_decl.h"
#endif

!*** local 
! arrays
#define IMTS its-1
#define IMTE ite+2
#define JMTS jts-1
#define JMTE jte+2
real,dimension(IMTS:IMTE,JMTS:JMTE):: tend, lfn1 ! region-sized with halo
! scalars
real::grad2,rr,tbound2
real, parameter::a=0.5, a1=1.0-a ! a=0 euler, a=0.5 heun

real::gradx,grady,aspeed,err,aerr,time_now
integer::ihs,ihe,jhs,jhe,irs,ire,jrs,jre
integer::i,j
character(len=128)msg
integer::nfirenodes,nfireline
real::sum_err,min_err,max_err,sum_aerr,min_aerr,max_aerr   

#ifdef DEBUG_OUT
integer,save::id=0
#endif

! constants
integer,parameter :: mstep=1000, printl=1
real, parameter:: zero=0.,one=1.,eps=epsilon(zero),tol=100*eps, &
    safe=2.,rmin=safe*tiny(zero),rmax=huge(zero)/safe

! f90 intrinsic function

intrinsic max,min,sqrt,nint,epsilon,tiny,huge
  
!*** executable

! bounds of the computed region 
irs=its
jrs=jts
ire=ite
if(ide.eq.ite)ire=ire+1
jre=jte
if(jde.eq.jre)jre=jre+1

write(msg,'(5(a,i5))')'prop_ls:',id,' tile  ',its,':',ite,',',jts,':',jte
call message(msg)
write(msg,'(5(a,i5))')'prop_ls:',id,' region',irs,':',ire,',',jrs,':',jre
call message(msg)

#ifdef DEBUG_OUT    
    call write_array_m(irs,ire,jrs,jre,ims,ime,jms,jme,lfn_in,'lfn_in',id)
#endif
    
    ! tend = F(lfn)

    ihs=max(irs-1,ids)   ! compute tend one beyond the region but not outside domain 
    ihe=min(ire,ide+1)
    jhs=max(jrs,jds)   ! compute tend one beyond the region but not outside domain 
    jhe=min(jre,jde+1)
    
    call  tend_ls(&
    ims,ime,jms,jme, &                       ! memory dims for lfn_in
    IMTS,IMTE,JMTS,JMTE, &                   ! memory dims for tend 
    ids,ide+1,jds,jde+1, &                   ! domain dims - where lfn exists
    ihs,ihe,jhs,jhe, &                       ! where tend computed
    ts,dx,dy,      &                          ! scalars in
    lfn_in, &                                   ! arrays in
    tbound, &                                ! scalars out 
    tend &                                   ! arrays out        
#ifdef SPEED_VARS_ARGS      /* extra arguments for normal_spread */
#include SPEED_VARS_ARGS
#else
#include "fr_sfire_params_args.h"
#endif
)

#ifdef DEBUG_OUT    
    call write_array_m(ihs,ihe,jhs,jhe,IMTS,IMTE,JMTS,JMTE,tend,'tendh',id)
#endif

    ! Euler method, the half-step
    ! lfn1 = lfn+dt*tend
    do j=jhs,jhe
        do i=ihs,ihe
            lfn1(i,j) = lfn_in(i,j) + dt*tend(i,j)
        enddo
    enddo
    
    ! tend = F(lfn1)

    call  tend_ls( &
    IMTS,IMTE,JMTS,JMTE, &                   ! memory dims for lfn
    IMTS,IMTE,JMTS,JMTE, &                   ! memory dims for tend 
    ids,ide+1,jds,jde+1, &                   ! domain dims - where lfn exists
    irs,ire,jrs,jre, &                       ! region dims - where is tend computed
    ts+dt,dx,dy,      &                          ! scalars in
    lfn1, &                                   ! arrays in
    tbound2, &                                ! scalars out 
    tend &                                  ! arrays out        
#ifdef SPEED_VARS_ARGS      /* extra arguments for normal_spread */
#include SPEED_VARS_ARGS
#else
#include "fr_sfire_params_args.h"
#endif
)
    
    tbound=min(tbound,tbound2)

    write(msg,'(a,f10.2,4(a,f7.2))')'prop_ls: time',ts,' dt=',dt,' bound',min(tbound,999.99), &
        ' dx=',dx,' dy=',dy
    call message(msg)
    if(dt>tbound)then
        write(msg,'(2(a,f10.2))')'prop_ls: WARNING: time step ',dt, &
        ' > bound =',tbound
        call message(msg)
    endif
    
    ! combine lfn1 and lfn_in + dt*tend -> lfn_out
    
    do j=jrs,jre
        do i=irs,ire
            lfn_out(i,j) = a1*lfn1(i,j) + a*(lfn_in(i,j) + dt*tend(i,j))
        enddo
    enddo      

    ! compute ignition time by interpolation
    ! the node was not burning at start but it is burning at end
    ! interpolate from the level functions at start and at end
    ! lfn_in   is the level set function value at time ts
    ! lfn_out  is the level set function value at time ts+dt
    ! 0        is the level set function value at time tign(i,j)
    ! thus assuming the level function is approximately linear =>
    ! tign(i,j)= ts + ((ts + td) - ts) * lfn_in / (lfn_in - lfn_out)
    !        = ts + dt * lfn_in / (lfn_in - lfn_out)

    time_now=ts+dt
    time_now = time_now + abs(time_now)*epsilon(time_now)*2.
    do j=jrs,jre
        do i=irs,ire
            ! interpolate the cross-over time
            if (.not. lfn_out(i,j)>0 .and. lfn_in(i,j)>0)then
                tign(i,j) = ts + dt * lfn_in(i,j) / (lfn_in(i,j) - lfn_out(i,j))
            endif
            ! set the ignition time outside of burning region
            if(lfn_out(i,j)>0.)tign(i,j)=time_now
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
    ! loop over right inside of the domain
    do j=jrs+1,jre-1
        do i=irs+1,ire-1
            if(lfn_out(i,j)>0.0)then   ! a point out of burning region
                if(lfn_out(i+1,j)<=0.or.lfn_out(i,j+1)<=0.or. & ! neighbor in burning region
                   lfn_out(i-1,j)<=0.or.lfn_out(i,j-1)<=0)then ! point next to fireline
                   gradx=(lfn_out(i+1,j)-lfn_out(i-1,j))/(2.0*dx) ! central differences
                   grady=(lfn_out(i,j+1)-lfn_out(i,j-1))/(2.0*dy)
                   grad2=sqrt(gradx*gradx+grady*grady)
                   aspeed = (lfn_in(i,j)-lfn_out(i,j))/(dt*max(grad2,rmin))                   
                    rr = speed_func(gradx,grady,i,j  &
#                   include "fr_sfire_params_args.h"
                    )
                   err=aspeed-rr
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
        (100.*nfirenodes)/((ire-irs+1)*(jre-jrs+1)),'% next to fireline',nfireline
    call message(msg)
    if(nfireline>0)then
        call print_stat_line('speed error',min_err,max_err,sum_err/nfireline)
        call print_stat_line('abs(speed error)',min_aerr,max_aerr,sum_aerr/nfireline)
    endif

    call print_2d_stats(irs,ire,jrs,jre,IMTS,IMTE,JMTS,JMTE,tend,'prop_ls:lvl fcn tend(1/s)')
    

end subroutine prop_ls

!
!*****************************
!

subroutine tend_ls( &
    lims,lime,ljms,ljme, &                   ! memory dims for lfn
    tims,time,tjms,tjme, &                   ! memory dims for tend 
    inds,inde,jnds,jnde, &                   ! domain - nodes where lfn defined
    ints,inte,jnts,jnte, &                   ! region - nodes where tend computed
    t,dx,dy,      &                          ! scalars in
    lfn, &                                   ! arrays in
    tbound, &                                ! scalars out 
    tend &                                  ! arrays out
#ifdef SPEED_VARS_ARGS      /* extra arguments for normal_spread */
#include SPEED_VARS_ARGS
#else
#include "fr_sfire_params_args.h"
#endif
)

implicit none
! purpose
! compute the right hand side of the level set equation

!*** arguments
integer,intent(in)::lims,lime,ljms,ljme,tims,time,tjms,tjme
integer, intent(in)::inds,inde,jnds,jnde,ints,inte,jnts,jnte
real,intent(in)::t                                     ! time
real,intent(in)::dx,dy                                 ! mesh step
real,dimension(lims:lime,ljms:ljme),intent(inout)::lfn ! level set function
real,intent(out)::tbound                               ! max allowed time step
real,dimension(tims:time,tjms:tjme),intent(out)::tend  ! tendency (rhs of the level set pde)
#ifdef SPEED_VARS_DECL      /* extra arguments for normal_spread */
#include SPEED_VARS_DECL
#else
#include "fr_sfire_params_decl.h"
#endif

!*** local 
real:: diffLx,diffLy,diffRx,diffRy, & 
   diffCx,diffCy,diff2x,diff2y,grad,rr
integer::i,j
! debug
!real, dimension(its:ite+1,jts:jte+1)::d_rr,d_nvx,d_nvy

! constants
real, parameter:: zero=0.,one=1.,eps=epsilon(zero),tol=100*eps, &
    safe=2.,rmin=safe*tiny(zero),rmax=huge(zero)/safe

! f90 intrinsic function

intrinsic max,min,sqrt,nint,epsilon,tiny,huge
  
!*** executable
    
    ! check array dimensions
    call check_mesh_2dim(ints-1,inte+1,jnts-1,jnte+1,lims,lime,ljms,ljme)
    call check_mesh_2dim(ints  ,inte+1,jnts  ,jnte+1,tims,time,tjms,tjme)
    
    call continue_at_boundary(1,1, &
    lims,lime,ljms,ljme, &                ! memory dims
    inds,inde,jnds,jnde, &                ! domain - nodes where lfn defined
    ints,inte,jnts,jnte, &                ! region - nodes where tend computed
    lfn)                                  ! array

    tbound=0    
    do j=jnts,jnte
        do i=ints,inte
            ! one sided differences
            diffRx = (lfn(i+1,j)-lfn(i,j))/dx
            diffLx = (lfn(i,j)-lfn(i-1,j))/dx
            diffRy = (lfn(i,j+1)-lfn(i,j))/dy
            diffLy = (lfn(i,j)-lfn(i,j-1))/dy
            ! const*central differences
            diffCx = lfn(i+1,j)-lfn(i-1,j)
            diffCy = lfn(i,j+1)-lfn(i,j-1)
    
            ! Godunov scheme: upwind differences, L or R or none    
            ! always test on > or < never = , much faster because of IEEE
            ! central diff >= 0 => take left diff if >=0, ortherwise 0
            ! central diff <= 0 => take right diff if <=0, ortherwise 0
            diff2x=0
            diff2y=0
            if (diffLx>0.and..not.diffCx<0)diff2x=diffLx
            if (diffRx<0.and.     diffCx<0)diff2x=diffRx
            if (diffLy>0.and..not.diffCy<0)diff2y=diffLy
            if (diffRy<0.and.     diffCy<0)diff2y=diffRy
            
            ! magnitude of the gradient
            grad=sqrt(diff2x*diff2x + diff2y*diff2y)

            ! get rate of spread

            rr = speed_func(diffCx,diffCy,i,j &
#           include "fr_sfire_params_args.h"
            )
            ! time step bound - CFL condition
            ! contribution of the normal term
            if (grad > 0.) then
                tbound = max(tbound,rr*(abs(diff2x)/dx+abs(diff2y)/dy)/grad)
            endif
            
            ! the rhs of the diff eq, a.k.a. lfn dot, a.k.a. the lfn "tendency"
            tend(i,j) = -rr*grad   ! normal term 
        enddo
    enddo        

    ! the final CFL bound
    tbound = 1/(tbound+tol)

end subroutine tend_ls

!
!**************************
!

real function speed_func(diffCx,diffCy,i,j &
#ifdef SPEED_VARS_ARGS      /* extra arguments for normal_spread */
#include SPEED_VARS_ARGS
#else
#include "fr_sfire_params_args.h"
#endif
)
!*** purpose
!    the level set method speed function
implicit none
!*** arguments
real, intent(in)::diffCx,diffCy  ! x and y coordinates of the direction of propagation
integer, intent(in)::i,j         ! indices of the node to compute the speed at
#ifdef SPEED_VARS_DECL      /* extra arguments for fire_ros */
#include SPEED_VARS_DECL
#else
#include "fr_sfire_params_decl.h"
#endif
!*** local
real::scale,nvx,nvy,speed,tanphi,r
real, parameter:: eps=epsilon(0.0)
!*** executable
            ! normal direction, from central differences
            scale=sqrt(diffCx*diffCx+diffCy*diffCy+eps) 
            nvx=diffCx/scale
            nvy=diffCy/scale
                      
            ! wind speed in direction of spread
            speed =  vx(i,j)*nvx + vy(i,j)*nvy
        
            ! slope in direction of spread
            tanphi =  dzfsdx(i,j)*nvx + dzfsdy(i,j)*nvy
    
            ! get rate of spread from wind speed and slope

            r = fire_ros(speed,tanphi,i,j &
#           include "fr_sfire_params_args.h"
            )
            ! write(10,*),i,j,nvx,nvy,speed,tanphi,r
            speed_func=max(r,0.0)

end function speed_func

end module module_fr_sfire_core
