module module_fr_sfire_speed
! speed function data

! 
integer::kw
parameter(kw=6)                           ! number of vertical velocities tracked
real,allocatable,dimension(:,:,:)::vx,vy  ! wind velocities
! will have dimension(ifms:ifme,jfms:jfme,kw) to keep dims consistent          
integer:: ifts,ifte,jfts,jfte             ! the actual fire grid extent

    contains
    ! the speed function
    
    subroutine normal_spread(t,n,rii,rjj,ims,ime,jms,jme,nvx,nvy,scale,speed)
    implicit none
    ! arguments
    real, intent(in)::t
    integer, intent(in)::n,ims,ime,jms,jme
    real,intent(in),dimension(ims:ime,jms:jme)::nvx,nvy,scale
    real, intent(in),dimension(n)::rii,rjj
    real,intent(out),dimension(n)::speed
    ! calls
    intrinsic nint
    !  internal
    integer i,j,k
 !  the speed function
 !  see module_fr_sfire_prop for argument description
    do k=1,n
        i=nint(rii(k))
        j=nint(rjj(k))
        speed(k) =  max(vx(i,j,1)*nvx(i,j) + vy(i,j,1)*nvy(i,j),0.0)
    enddo
    end subroutine normal_spread

    end module module_fr_sfire_speed
