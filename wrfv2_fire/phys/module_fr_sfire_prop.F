module module_fr_sfire_prop

contains

subroutine prop_ls(ids,ide,jds,jde,ims,ime,jms,jme,phi,tc,ts,td,dx,dy)
use module_fr_sfire_util
use module_fr_sfire_speed
implicit none

!*** purpose: advance level function in time

! Jan Mandel August 2007
! Special credit: Minjeong Kim observed which methods from tolboxLS actually work

!*** description
!
! Propagation of closed curve by a level function method. The level function
! phi is defined by its values at the nodes of a rectangular grid. 
! The area where phi < 0 is inside the curve. The curve is 
! described implicitly by phi=0. Points where the curve intersects gridlines
! can be found by linear interpolation from nodes.
!
! The level function is advanced from time ts to time ts + dt. 
!
! The level function should be initialized to (an approximation of) the signed
! distance from the curve. If the initial curve is a circle, the initial level
! function is simply the distance from the center minus the radius.
! 
! The curve moves outside with speed rr in normal direction and by
! advection by velocity field (vvx,vvy), given by values at grid nodes.
! Thus, the speed of the curve evolution in the normal direction is
!  rr + (vvx,vvy) dot normal vector. This speed and advection are computed by
! modifying an external speed r and advection field (vx,vy) to match the speed
! in the normal direction given by call to subroutine normal_spread in 
! module_fr_sfire_speed.
! 
! ------------------------------------
! Based on S. Osher and R. Fedkiw, Level set methods and dynamic implicit surfaces,
! Springer, 2003, Sec. 6.4, as implemented in toolboxLS for Matlab by 
! I. Mitchell, A toolbox of Level Set Methods (Version 1.1), TR-2007-11,
! Dept. Computer Science, University of British Columbia, 2007
! http://www.cs.ubc.ca/\~mitchell/ToolboxLS
! 
! This code can reproduce (up to rounding) spinStarDemo from 
! toolboxLS with accuracy='low'. See prop_ls_readme.txt how.
!   
! Method: Upwinding based on 1st order one-sided differences in space, 
! Euler method in time. Godunov method for the normal motion combined with
! simple upwinding for advection. The timestep is set automatically based on
! CFL condition. For a straight segment in a constant field and locally linear
! level function, the method reduces to the exact normal motion. The advantage of 
! the level set method is that it treats automatically special cases such as
! the curve approaching itself and merging components of the area inside the curve.
!
! IMPORTANT NOTE: This code passed all test except for a circular curve
! moving in a circular field with center outside of the curve at the distance
! of several diameters. The curve should travel on a circular path and grow;
! instead it slowly shrinks. This seems to be an artefact of the low order
! of the method. In toolboxLS, one has to set accuracy='high' to make it grow.
! But this involves a high-order Runge-Kutta scheme in time and a quite complicated 
! high-order ENO method in space, and is much more expensive.
  
!*** arguments 

! ims,ime,jms,jme   in    array dimensions
! ids,ide,jds,jde   in    grid dimensions in the array, in cells (in nodes, +1)
! phi               inout the level function
! tc                inout the time the contour=zero level set crossed the node
! ts                in    start time
! td                in    end time is ts+td (given this way because of rounding)
! dx,dy             in    grid spacing

!*** global
! in module_fr_sfire_speed:
!
! vx,vy             in    background advection speed
! r                 in    background spread rate in normal direction
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
!   speed = r + max(vx*nvx + vy*nvy,0.0)
      
integer,intent(in)::ims,ime,jms,jme,ids,ide,jds,jde
real,dimension(ims:ime,jms:jme),intent(inout)::phi,tc
real,intent(in)::dx,dy,ts,td

!*** local 
! arrays
real,dimension(ids:ide+1,jds:jde+1):: & ! same mesh as phi, but declare only the mesh used
   tend,diffLx,diffLy,diffRx,diffRy, & 
   diffCx,diffCy, &
   tbound_n, tbound_ax, tbound_ay, &
   nvx, nvy,speed, scale
real, dimension((jde-jds+2)*(ide-ids+2))::rii,rjj,speedp
! scalars
real:: diff2x,diff2y, &
   a,tend_n,tend_a,  &
   rr, vvx, vvy,grad, &
   m_tbound_n, m_tbound_ax, m_tbound_ay, &
   t,dt,tr,tbound,phi_old,phi_new
integer::istep
! integer::m1,m,n1,n
integer:: i,j,k,kk
! constants
integer:: mstep
parameter(mstep=1000)
real:: zero,one,eps,tol
parameter(zero=0.0,one=1.0,eps=epsilon(zero),tol=100*eps)
character(len=128)msg

! f90 intrinsic function

intrinsic max,min,sqrt,nint,epsilon
  
!*** executable

call check_mesh_2dim(ids,ide+1,jds,jde+1,ims,ime,jms,jme)

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

do while ( tr + td*tol < td .and. istep < mstep )

    istep=istep+1
    t = ts + tr  ! advance tr instead of t because of rounding

    ! one sided differences
    ! we waste a little and store them separately to make the code more
    ! readable, and to allow for higher order scheme in future if needed
    ! allow for general array bounds n1:n,m1:m - may be useful in parallel
    
    call meshdiff_2d(        &           ! get all 4 one-sided differences
        ids,ide,  jds,jde ,  &           ! mesh area used (in cells, end +1)
        ims,ime,  jms,jme,   &           ! dimensions of phi 
        ids,ide+1,jds,jde+1, &           ! dimensions of the outputs
        dx,dy,               &           ! mesh spacing
        phi,                 &           ! input
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
    call normal_spread(t,dx,dy,k,rii,rjj,ids,ide,jds,jde,nvx,nvy,scale,speedp) 
    do kk=1,k
       i=nint(rii(kk))
       j=nint(rjj(kk))
       speed(i,j)=speedp(kk)
    enddo
    !print *,'speed: ',speed(1:3,1:3)
    !speed = r + max(vx*nvx + vy*nvy,0.0)
  
    ! recover advection vv and spread r, transition between:
    ! r = 0 & (normal,v) > const*speed => vv=speed*v/(normal,v) & rr=0
    ! speed >> (normal,v) => vv=0 & rr=speed

    do j=jds,jde+1
        do i=ids,ide+1
            
            ! normal component of the advection velocity large
            if(2*max(nvx(i,j) * vx(i,j) + nvy(i,j) * vy(i,j),eps)>speed(i,j)) then
                a=1  ! spread by advection
            else
                a=0  ! spread by normal motion
            endif ! may want a transition?
            rr=speed(i,j)*(1-a)
            vvx = vx(i,j) * a
            vvy = vy(i,j) * a
            
            ! Godunov scheme: choose the upwind direction, L or R or none    
            ! always test on > or < never = , much faster because of IEEE
     
            diff2x=0
            diff2y=0
            if (.not.diffLx(i,j)<0.and..not.diffCx(i,j)<0)diff2x=diff2x+diffLx(i,j)
            if (.not.diffRx(i,j)>0.and.     diffCx(i,j)<0)diff2x=diff2x+diffRx(i,j)
            if (.not.diffLy(i,j)<0.and..not.diffCy(i,j)<0)diff2y=diff2y+diffLy(i,j)
            if (.not.diffRy(i,j)>0.and.     diffCy(i,j)<0)diff2y=diff2y+diffRy(i,j)
    
            ! magnitude of the gradient
            ! do not use **2 who knows if the compiler is smart enough  
            grad=sqrt(diff2x*diff2x + diff2y*diff2y)
    
            ! time step bound - CFL condition
            ! contribution of the normal term
            if (grad > 0) then
                tbound_n(i,j) = rr*(abs(diff2x)/dx+abs(diff2y)/dy)/grad
            else
                tbound_n(i,j) = 0
            endif
            ! contribution of the advection term
            tbound_ax(i,j)=abs(vvx)
            tbound_ay(i,j)=abs(vvy)
            
            ! tendencies
            tend_n = -rr*grad   ! normal term rhs
            tend_a = -(diffLx(i,j)*max(vvx,zero)+diffRx(i,j)*min(vvx,zero)+ & ! advection term
               diffLy(i,j)*max(vvy,zero)+diffRy(i,j)*min(vvy,zero)) ! advection term
            tend(i,j)=tend_n + tend_a

            !  trailing edge correction - do not allow fireline to go backwards
            ! THIS IS DIFFERENT FROM THE TOOLBOX!!
            if(phi(i,j)<=0.0)then
                tend(i,j)=min(tend(i,j),-0.5*one)
            endif           
        enddo
    enddo        
    
    ! max time bound in each category
    ! in MPI replace by reduction operation
    m_tbound_n=0
    m_tbound_ax=0
    m_tbound_ay=0
    do j=jds,jde+1
        do i=ids,ide+1
             m_tbound_n=max(m_tbound_n,tbound_n(i,j))
             m_tbound_ax=max(m_tbound_n,tbound_ax(i,j))
             m_tbound_ay=max(m_tbound_n,tbound_ay(i,j))
        enddo
    enddo        
         
    ! the final CFL bound and the timestep
    tbound = 1/(m_tbound_n+m_tbound_ax/dx+m_tbound_ay/dy+tol)
    dt = min(td-tr, 0.5*tbound)
    !print *,'prop_ls:step,time,dt=',istep,t,dt    

    ! the rhs of the diff eq, a.k.a. phi dot, a.k.a. the phi "tendency"
    ! 0. because max requires both arguments same type (real) 
        
    do j=jds,jde+1
        do i=ids,ide+1
            phi_old = phi(i,j)
            phi_new = phi(i,j) + dt*tend(i,j) 
            ! interpolate the cross-over time
            if(phi_old>0 .and. .not. phi_new>0)then
                ! the node was not burning at start but it is burning at end
                ! interpolate from the level functions at start and at end
                ! phi_old is the level function value at time t
                ! phi_new is the level function value at time t+dt
                ! 0       should the level function value at time tc(i,j)
                ! thus assuming the level function is approximately linear =>
                ! tc(i,j)=t + ((t + td) - t) * phi_old / (phi_old - phi_new)
                tc(i,j) = t - phi_old / tend(i,j)
            endif
            ! advance one step
            phi(i,j) = phi_new
        enddo
    enddo

    tr = tr + dt
    
end do !while

if (abs(tr - td) > 2*tol .or. istep >= mstep) then
    write(msg,*)'prop_ls: time advanced by ',tr,' of ',td
    call message(msg)
    write(msg,*)'diff ',tr-td, ' in ',istep,' steps of ',mstep
    call message(msg)    
    call crash('prop_ls: end time not reached or max steps exceeded')
endif

end subroutine prop_ls

end module module_fr_sfire_prop
