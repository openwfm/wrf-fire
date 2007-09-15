module module_fr_sfire_speed

! speed function data - will be assigned by model to actual arrays so that they can be used here
 
real,pointer,dimension(:,:):: vx,vy                    ! wind velocity
real,pointer,dimension(:,:):: dzfsdx,dzfsdy            ! terrain gradient
real,pointer,dimension(:,:):: bbb,betafl,phiwc,r_0     ! spread formula coefficients
integer, pointer, dimension(:,:):: ischap    


contains

subroutine allocate_fire_params(ifms,ifme,jfms,jfme)
implicit none
integer, intent(in):: ifms,ifme,jfms,jfme
allocate(vx(ifms:ifme,jfms:jfme))
allocate(vy(ifms:ifme,jfms:jfme)) 
allocate(dzfsdx(ifms:ifme,jfms:jfme)) 
allocate(dzfsdy(ifms:ifme,jfms:jfme)) 
allocate(bbb(ifms:ifme,jfms:jfme)) 
allocate(betafl(ifms:ifme,jfms:jfme)) 
allocate(phiwc(ifms:ifme,jfms:jfme)) 
allocate(r_0(ifms:ifme,jfms:jfme)) 
end subroutine allocate_fire_params

subroutine deallocate_fire_params
implicit none
deallocate(vx)
deallocate(vy) 
deallocate(dzfsdx) 
deallocate(dzfsdy) 
deallocate(bbb) 
deallocate(betafl) 
deallocate(phiwc) 
deallocate(r_0) 
end subroutine deallocate_fire_params



   
! the speed function
    
subroutine normal_spread(t,dx,dy,n,rii,rjj,ids,ide,jds,jde,nvx,nvy,scale,ros)
use module_fr_sfire_util
implicit none
!  the speed function
!  see module_fr_sfire_prop for argument description

!*** arguments
real, intent(in)::t,dx,dy  ! currently not used
integer, intent(in)::n,ids,ide,jds,jde
real,intent(in),dimension(ids:ide+1,jds:jde+1)::nvx,nvy,scale
real, intent(in),dimension(n)::rii,rjj
real,intent(out),dimension(n)::ros

!*** calls
intrinsic nint,max,min

!***  internal
integer i,j,k
real tanphi,speed
real,dimension(ids:ide+1,jds:jde+1)::ros_all

!*** executable
!  compute speed at all nodes
do j=jds,jde+1
    do i=ids,ide+1

        ! wind speed in direction of spread
        speed =  vx(i,j)*nvx(i,j) + vy(i,j)*nvy(i,j)
        
        ! slope in direction of spread
        tanphi =  dzfsdx(i,j)*nvx(i,j) + dzfsdy(i,j)*nvy(i,j)
  
        ! get rate of spread from wind speed and slope
         
        ros_all(i,j) = fire_ros(speed,tanphi,i,j)

    enddo
enddo

! interpolate to the requested points
do k=1,n
    ros(k)=interp(ids,ide,jds,jde,ids,ide+1,jds,jde+1,rii(k),rjj(k),ros_all)
enddo
    
end subroutine normal_spread


real function fire_ros(speed,tanphi,i,j)

! use module_fr_sfire_speed  ! we are contained in this module
implicit none

! copied from wf2_janice 
! changes only:
!   required by f90
!   0.5*(speed + abs(speed)) -> max(speed,0.)
!   index l -> j 
!   the indices are node not cell but that changes nothing
!   took out some prints
!   made into a function
!   argument fuelloadm never used??
!   not using nfuel_cat here - cell info was put into module arrays
!   betaop is module variable instead of local, in  case when needed 
!       (set in module_fr_sfire_model/fire_startup)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     ... calculates fire spread rate with mcarthur formula or behave
!           using fuel type of fuel cell
!
!      
!         m/s =(ft/min) *.3048/60. =(ft/min) * .00508   ! conversion rate
!         ft/min = m/s * 2.2369 * 88. = m/s *  196.850 ! conversion rate
!      
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!*** arguments
real, intent(in):: speed, tanphi ! windspeed and slope in the directino normal to the fireline
integer, intent(in)::i,j         ! node mesh coordinates

!*** local
real:: umid, phis, phiw, rosm, spdms, umidm, sp_n
!real:: e,  c
integer:: ibeh
parameter(ibeh=1)

!*** executable

!
if (ischap(i,j) .eq. 0) then            ! not chaparral
    if (ibeh .eq. 1) then                ! behave
!       ... if wind is 0 or into fireline, phiw = 0, &this reduces to backing ros.
        spdms = max(speed,0.)          ! unscale model to m/s 10*.5
        umidm = min(spdms,10.)       ! max input wind spd is 10 m/s   !param!
        umid = umidm * 196.850                    ! m/s to ft/min
        !  eqn.: phiw = c * umid**bbb(i,j) * (betafl(i,j)/betaop)**(-e) ! wind coef
        phiw = umid**bbb(i,j) * phiwc(i,j) ! wind coef
        phis=0  !*** jm added ??
        if (tanphi .gt. 0.) then
            phis = 5.275 *(betafl(i,j))**(-0.3) *tanphi**2   ! slope factor
        endif
        rosm = r_0(i,j)*(1. + phiw + phis)  * .00508 ! spread rate, m/s
            
        !write (6,'(x,a,2i4,4(x,e12.3) )') 'i,l,rosm,r0,phiw,phis=', &
        !   i,l,rosm,r_0(i,j),phiw,phis
        !if (rosm .gt. 1) write (6,*) 'speed=',speed,' tanphi=',tanphi
!
    else                                   !macarthur formula
        rosm = 0.18*exp(0.8424*max(speed,0.))
    endif
!
else   ! chaparral
!        .... spread rate has no dependency on fuel character, only windspeed.
    spdms = max(speed,0.)       ! unscale model to m/s 10*.5
    rosm = 1.2974 * spdms**1.41       ! spread rate, m/s
    ! note: backing ros is 0 for chaparral without setting nozero value below
    sp_n=.03333    ! chaparral backing fire spread rate 0.033 m/s   ! param!
    rosm= max(rosm, sp_n)   ! no less than backing r.o.s.
endif
!
!     ----------note!  put an 6 m/s cap on max spread rate -----------
rosm= min(rosm, 6.)         ! no faster than this cap   ! param !

!     ... to rescale to veloc. carried by model, mult x (svel*snorm(1,3))= .1
!jm: huh ???
      fire_ros = 0.1*rosm
!
      return
end function fire_ros 


end module module_fr_sfire_speed
