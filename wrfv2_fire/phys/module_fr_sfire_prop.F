! WRF MEDIATION LAYER: PHYSICS

module module_fr_sfire_prop

!#define DEBUG_OUT

contains

subroutine prop_ls(ids,ide,jds,jde, &                       ! domain dims
                   ims,ime,jms,jme, &                       ! memory dims
                   num_tiles,i_start,i_end,j_start,j_end, & ! tiling
                   ts,td,dx,dy,     &                       ! scalars in
                   lfn,tc           &                       ! arrays inout                   
#ifdef SPEED_VARS_ARGS      /* extra arguments for normal_spread */
#include SPEED_VARS_ARGS
#else
#include "fr_sfire_params_args.inc"
#endif
                   )
use module_fr_sfire_util
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
#include "fr_sfire_params_decl.inc"
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
#include "fr_sfire_params_args.inc"
#endif    
    ) 
    do kk=1,k
       i=nint(rii(kk))
       j=nint(rjj(kk))
       speed(i,j)=speedp(kk)
    enddo

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
        write(msg,'(7(a,f15.6))')'prop_ls:time',t,' dt',dt, &
        ' avg speed',avg_2darray(1,k,1,1,1,k,1,1,speedp), &
        ' avg tend',avg_2darray(ids,ide+1,jds,jde+1,ims,ime,jms,jme,tend), &
        ' avg wind',avg_2darray_vec(ids,ide+1,jds,jde+1,ims,ime,jms,jme,vx,vy)
        call message(msg)
    endif
    
end do !while

if (printl>=1)then
    write(msg,*)'prop_ls: total number of time steps ',istep
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

end module module_fr_sfire_prop
