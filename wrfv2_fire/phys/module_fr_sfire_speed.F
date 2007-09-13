module module_fr_sfire_speed

! speed function data - will be assigned by model to actual arrays so that they can be used here
 
real,pointer,dimension(:,:):: &
    vx,vy,                   &  ! wind velocity
    dzfsdx,dzfsdy,           &  ! terrain gradient
    bbb,betafl,phiwc,r_0      ! spread formula coefficients
    
real:: betaop                 ! spread formula coefficients

contains
! the speed function
    
subroutine normal_spread(t,dx,dy,n,rii,rjj,ids,ide,jds,jde,nvx,nvy,scale,speed)
use module_fr_sfire_util
implicit none
!  the speed function
!  see module_fr_sfire_prop for argument description

!*** arguments
real, intent(in)::t,dx,dy  ! currently not used
integer, intent(in)::n,ids,ide,jds,jde
real,intent(in),dimension(ids:ide+1,jds:jde+1)::nvx,nvy,scale
real, intent(in),dimension(n)::rii,rjj
real,intent(out),dimension(n)::speed

!*** calls
intrinsic nint,max,min

!***  internal
integer i,j,k
real spd_ms,spd_fm,phiw,phis,tanphi
real,dimension(ids:ide+1,jds:jde+1)::ros

!*** executable
!  compute speed at all nodes
do j=jds,jde+1
    do i=ids,ide+1

        ! wind speed in direction of spread
        spd_ms =  vx(i,j)*nvx(i,j) + vy(i,j)*nvy(i,j)
        
        ! slope in direction of spread
        tanphi =  dzfsdx(i,j)*nvx(i,j) + dzfsdy(i,j)*nvy(i,j)
  
        ! get rate of spread from wind speed and slope
        
        spd_ms = max(spd_ms,0.0)          ! min 0, no backward spread
        spd_ms = min(spd_ms,10.)          ! max input wind spd is 10 m/s  !param
        spd_fm = spd_ms * 196.850         ! convert wind spd from m/s to ft/min

        ! wind factor : phiw = c * spd_fm**bbb(i,j) * (betafl(i,j)/betaop)**(-e)
        phiw = spd_fm**bbb(i,j) * phiwc(i,j) ! wind coef  

        ! --- slope factor

        phis = 0.
        IF (tanphi  >  0.) THEN
            phis = 5.275 *(betafl(i,j))**(-0.3) *tanphi**2   ! slope factor 
        END IF

        ! --- spread rate (m/s)

        ros(i,j) = r_0(i,j) * (1. + phiw + phis) * .00508

        !WRITE (msg,'(x,a,2i4,5(x,e12.3) )') 'i,j,ros,r_0,phiw,phis=',  &
        !                                      i,j,ros,r_0(i,j),phiw,phis
        !CALL message (msg )
    enddo
enddo

! interpolate to the requested points
do k=1,n
    speed(k)=interp(ids,ide,jds,jde,ids,ide+1,jds,jde+1,rii(k),rjj(k),ros)
enddo
    
end subroutine normal_spread

end module module_fr_sfire_speed
