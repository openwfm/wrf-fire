module module_fr_propagate

contains

subroutine prop_ls(m1,m,n1,n,phi,t,ts,te,vx,vy,r,dx,dy)
  implicit none
  
  !*** purpose: advance level function in time
  
  ! Jan Mandel and Minjeong Kim, August 2007
  
  !*** description
  !
  ! Propagation of closed curve by a level function method. The level function
  ! phi is defined by its values at the nodes of a rectangular grid. 
  ! The area where phi < 0 is inside the curve. The curve is 
  ! described implicitly by phi=0. Points where the curve intersects gridlines
  ! can be found by linear interpolation from nodes.
  !
  ! The level function is advanced from time ts to time te (up to rounding). 
  ! At exit, the actual time t is returned, which should essentially equal ts. 
  !
  ! The level function should be initialized to (an approximation of) the signed
  ! distance from the curve. If the initial curve is a circle, the initial level
  ! function is simply the distance from the center minus the radius.
  ! 
  ! The curve moves outside with speed r in normal direction and by
  ! advection by velocity field (vx,vy), given by values at grid nodes.
  ! Thus, the speed of the curve evolution in the normal direction is
  !  r + (vx,vy) dot normal vector. 
  ! 
  ! Currently r is a scalar but it could be easily given by a grid array. All that is
  ! needed is to declare it so. 
  !
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
  
  ! m1,m,n1,n grid bounds (usually n1=m1=1)
  ! phi     the level function
  ! t       actual end time
  ! ts      star time
  ! te      end time
  ! vx,vy   velocity for advection
  ! r       normal speed, must be positive.
  ! dx,dy   grid spacing
  
  integer,intent(in)::m1,m,n1,n
  real,dimension(m1:m,n1:n),intent(inout)::phi
  real,intent(out)::t
  real,dimension(m1:m,n1:n),intent(in)::vx,vy,r
  real,intent(in)::dx,dy,ts,te

  !*** local arrays, same dimension as phi
  
  real,dimension(m1:m,n1:n)::tend,grad, &
    diff2x,diff2y, &
    diffLx,diffLy,diffRx,diffRy, &
    flowLx,flowRx,flowLy,flowRy, &
    diffCx,diffCy, &
    tend_n,tend_a, &
    tbound_np

  !*** local variables
  
  integer::istep,mstep
  real::dt,tol,tbound,tbound_n,tbound_a,zero,one
  parameter(zero=0.0,one=1.0)
  
  ! f90 intrinsic function
  
  intrinsic max,min,maxval,sqrt,lbound,ubound
    
  !*** executable

  !print *,'prop_ls:m1,m,n1,n,ts,te,r,dx,dy=', &
  !               m1,m,n1,n,ts,te,r,dx,dy

  t=ts
  tol=6.*10**(-14) ! 300*2.22*10^-16
  istep = 0
  mstep = 1000
  
  do while ( t < (te-tol) .and. istep < mstep )

    istep=istep+1

    ! one sided differences
    ! we waste a little and store them separately to make the code more
    ! readable, and to allow for higher order scheme in future if needed
    ! allow for general array bounds n1:n,m1:m - may be useful in parallel
  
    diffLx(m1+1:m,:) = (phi(m1+1:m,:)-phi(m1:m-1,:))/dx    
    diffRx(m1:m-1,:) = diffLx(m1+1:m,:)    
    diffLx(m1  ,:)   = diffLx(m1+1,:)
    diffRx(m    ,:)  = diffLx(m,:)
    diffLy(:,n1+1:n) = (phi(:,n1+1:n)-phi(:,n1:n-1))/dy    
    diffRy(:,n1:n-1) = diffLy(:,n1+1:n)    
    diffLy(:  ,n1)   = diffLy(:,n1+1)
    diffRy(:  ,n)    = diffLy(:,n)
    
    ! 2 times central differences
  
    diffCx=diffLx + diffRx
    diffCy=diffLy + diffRy

    ! Godunov scheme: choose the upwind direction, L or R or none    
    ! always test on > or < never = , much faster because of IEEE
    
    ! flowLx = (diffLx >=0 & diffCx >=0)
  
    where (.not. diffLx<0 .and. .not. diffCx<0)
        flowLx=1
    elsewhere 
        flowLx=0
    end where
    
    ! flowRx=(diffRx<=0 & diffCx<0)
  
    where (.not. diffRx>0 .and. diffCx<0)
        flowRx=1
    elsewhere
        flowRx=0
    end where
    
    ! flowLy = (diffLy >=0 & diffCy >=0)
  
    where (.not. diffLy<0 .and. .not. diffCy<0)
        flowLy=1
    elsewhere
        flowLy=0
    end where
    
    ! flowRy=(diffRy<=0 & diffCy<0)
  
    where (.not. diffRy>0 .and. diffCy<0)
        flowRy=1
    elsewhere
        flowRy=0
    end where
    
    ! Godunov scheme: choose the proper one sided difference or zero
  
    diff2x=diffLx*flowLx + diffRx*flowRx
    diff2y=diffLy*flowLy + diffRy*flowRy
    
    ! magnitude of the gradient
    ! do not use **2 who knows if the compiler is smart enough
  
    grad=sqrt(diff2x*diff2x + diff2y*diff2y)
    
    ! time step bound - CFL condition
    
    ! contribution of the normal term

    where (grad > 0) tbound_np = r*(abs(diff2x)/dx+abs(diff2y)/dy)/grad
    tbound_n = maxval(tbound_np, grad>0) ! do not use undefined entries
    
    ! contribution of the advection term

    tbound_a = maxval(abs(vx))/dx+maxval(abs(vy))/dy
    
    ! the final CFL bound and the timestep

    tbound = 1/(tbound_n+tbound_a+tol)
    dt = min(te-t, 0.5*tbound)
    
    ! the rhs of the diff eq, a.k.a. phi dot, a.k.a. the phi "tendency"
    ! 0. because max requires both arguments same type (real) 

    tend_n = -r*grad   ! normal term
    tend_a = -(diffLx*max(vx,zero)+diffRx*min(vx,zero)+ & ! advection term
               diffLy*max(vy,zero)+diffRy*min(vy,zero)) ! advection term
    tend=tend_n + tend_a

    !  trailing edge correction - do not allow fireline to go backwards
    ! THIS IS DIFFERENT FROM THE TOOLBOX!!
    where (phi<=0.0) tend=min(tend,-0.5*one)

    !print *,'prop_ls:step,time,dt=',istep,t,dt    

    ! advance one step

    phi = phi + dt*tend
    t = t + dt
    
  end do !while
    
  !if (istep >= mstep) print *,'max iteration reached'
  !print *,'prop_ls:step,ts,te=',istep,ts,te
  
end subroutine prop_ls

end module module_fr_propagate
