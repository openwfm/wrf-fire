    module module_fr_sfire_speed
    ! speed function data
    real,pointer,dimension(:,:)::vx,vy,r

    contains
    ! the speed function
    
    subroutine normal_spread(t,dx,dy,n,rii,rjj,ids,ide,jds,jde,nvx,nvy,scale,speed)
    use module_fr_sfire_util
    implicit none
!   arguments
    real, intent(in)::t,dx,dy ! not used right now
    integer, intent(in)::n,ids,ide,jds,jde
    real,intent(in),dimension(ids:ide+1,jds:jde+1)::nvx,nvy,scale
    real, intent(in),dimension(n)::rii,rjj
    real,intent(out),dimension(n)::speed
    ! calls
    intrinsic nint
    !  internal
    integer i,j,k
    real,dimension(ids:ide+1,jds:jde+1)::sn
 !  the speed function
 !  see module_fr_sfire_prop for argument description
 !  compute speed at all nodes
    do j=jds,jde+1
        do i=ids,ide+1
            sn(i,j) = r(i,j) + max(vx(i,j)*nvx(i,j) + vy(i,j)*nvy(i,j),0.0)
        enddo
    enddo
 ! interpolate to the requested points
    do k=1,n
        speed(k)=interp(ids,ide,jds,jde,ids,ide+1,jds,jde+1,rii(k),rjj(k),sn)
    enddo
    end subroutine normal_spread

    end module module_fr_sfire_speed
