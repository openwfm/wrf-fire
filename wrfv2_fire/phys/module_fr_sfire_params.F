module module_fr_sfire_params

! fire model parameter arrays
 
real,allocatable,save,dimension(:,:):: vx,vy                    ! (node) wind velocity (m/s)
real,allocatable,save,dimension(:,:):: dzfsdx,dzfsdy            ! (node) terrain gradient (1)
real,allocatable,save,dimension(:,:):: bbb,betafl,phiwc,r_0     ! (node) spread formula coefficients
real,allocatable,save,dimension(:,:):: fgip                     ! (cell) init mass of surface fuel (kg/m^2)
integer,allocatable,save,dimension(:,:):: ischap                ! (node) .ne.0 if chapparal
real,allocatable,save,dimension(:,:):: fuel_time                ! (cell) fuel burns as exp(-t/fuel_time)

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
allocate(fgip(ifms:ifme,jfms:jfme))
allocate(ischap(ifms:ifme,jfms:jfme))
allocate(fuel_time(ifms:ifme,jfms:jfme))
!print *,'allocated: ',ifms,ifme,jfms,jfme
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
deallocate(fgip)
deallocate(ischap)
deallocate(fuel_time) 
end subroutine deallocate_fire_params

end module module_fr_sfire_params
