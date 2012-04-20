! WRF:MEDIATION_LAYER:FIRE_MODEL

! This is WRF interface driver for SFIRE, the fire module in WRF-Fire.
! Please see module_fr_sfire_driver.F for acknowledgements.


#define DEBUG_OUT

module module_fr_sfire_driver_wrf
! wrf-specific driver

use module_fr_sfire_driver
use module_fr_sfire_atm
USE module_utility, only: WRFU_TimeInterval,WRFU_TimeIntervalGet, WRFU_SUCCESS
implicit none

contains

subroutine sfire_driver_em_init (grid , config_flags               & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe)

    ! stub to call sfire_driver_em with irun=0 and omit last 3 args

    USE module_domain , only: domain , get_ijk_from_subgrid , &
                              domain_get_time_since_sim_start , &
                              domain_get_time_step 
    USE module_configure , only : grid_config_rec_type
    implicit none

    TYPE(domain) , TARGET          :: grid   ! data
    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe

    ! local
    integer :: &  ! fire mesh sizes
             ifds,ifde, kfds,kfde, jfds,jfde,                              &
             ifms,ifme, kfms,kfme, jfms,jfme,                              &
             ifps,ifpe, kfps,kfpe, jfps,jfpe                              
    real::time_step_start, dt ! dummies, avoid uninitialized
    ! dummies

    real,dimension(1,1,1)::rho,z_at_w,dz8w

    call message('sfire_driver_em_init: SFIRE initialization start')

    ! get fire mesh dimensions
    CALL get_ijk_from_subgrid (  grid ,                   &
                            ifds,ifde, jfds,jfde,kfds,kfde,                        &
                            ifms,ifme, jfms,jfme,kfms,kfme,                        &
                            ifps,ifpe, jfps,jfpe,kfps,kfpe) 

    ! times in seconds
    time_step_start=TimeInterval2Sec(domain_get_time_since_sim_start(grid)) 
    dt=TimeInterval2Sec(domain_get_time_step(grid))

    call sfire_driver_em ( grid , config_flags               & 
            ,time_step_start,dt                                 &
            ,1,2,0                        & ! ifun start, end, test steps
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ) 

    call message('sfire_driver_em_init: SFIRE initialization complete')

end subroutine sfire_driver_em_init

!
!***
!

subroutine sfire_driver_em_step (grid , config_flags               & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,rho,z_at_w,dz8w ) 

    ! stub to call sfire_driver_em 

    USE module_domain, only: domain , get_ijk_from_subgrid , &
                              domain_get_time_since_sim_start , &
                              domain_get_time_step 
    USE module_configure , only : grid_config_rec_type
    USE module_fr_sfire_util, only : fire_test_steps
    implicit none

    TYPE(domain) , TARGET          :: grid   ! data
    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe
    real,dimension(ims:ime, kms:kme, jms:jme),intent(in)::rho,z_at_w,dz8w


    ! local
    integer :: &  ! fire mesh sizes
             ifds,ifde, kfds,kfde, jfds,jfde,                              &
             ifms,ifme, kfms,kfme, jfms,jfme,                              &
             ifps,ifpe, kfps,kfpe, jfps,jfpe                              
    integer :: its,ite,jts,jte,kts,kte            ! atm tile
    integer:: ij 
    real::time_step_start,dt

    integer::fire_time_step_ratio,itime_step,i,j
    real,dimension( ips:ipe , jps:jpe ) :: grnhfx_save, grnqfx_save,    &
                                           canhfx_save, canqfx_save

    ! executable

    call message('sfire_driver_em_step: SFIRE step start')
    
    ! times in seconds

    ! get fire time step refinement from namelist
    fire_time_step_ratio=config_flags%fire_time_step_ratio
    
    if(fire_time_step_ratio.lt.1)then
        call crash('fire_time_step_ratio must be >= 1')
    endif

    time_step_start=TimeInterval2Sec(domain_get_time_since_sim_start(grid)) 
    dt=TimeInterval2Sec(domain_get_time_step(grid))/fire_time_step_ratio
    

    ! get fire mesh dimensions
    CALL get_ijk_from_subgrid (  grid ,                   &
                            ifds,ifde, jfds,jfde,kfds,kfde,                        &
                            ifms,ifme, jfms,jfme,kfms,kfme,                        &
                            ifps,ifpe, jfps,jfpe,kfps,kfpe) 

    ! save fluxes for tendency
    grnhfx_save(:,:)=0.
    grnqfx_save(:,:)=0.
    canhfx_save(:,:)=0.
    canqfx_save(:,:)=0.

    ! fire time step loop
    do itime_step = 1,fire_time_step_ratio

    call sfire_driver_em ( grid , config_flags               & 
            ,time_step_start,dt                                 &
            ,3,6,fire_test_steps                                &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ) 

        ! accumulate fluxes for atmospheric tendency
        do j=jps,jpe
            do i=ips,ipe
                grnhfx_save(i,j)=grnhfx_save(i,j) + grid%grnhfx(i,j)
                grnqfx_save(i,j)=grnqfx_save(i,j) + grid%grnqfx(i,j)
                canhfx_save(i,j)=canhfx_save(i,j) + grid%canhfx(i,j)
                canqfx_save(i,j)=canqfx_save(i,j) + grid%canqfx(i,j)
            enddo
        enddo

        time_step_start=time_step_start+dt
    enddo

        ! copy fluxes back to grid structure
        do j=jps,jpe
            do i=ips,ipe
                grid%grnhfx(i,j)=grnhfx_save(i,j)/fire_time_step_ratio
                grid%grnqfx(i,j)=grnqfx_save(i,j)/fire_time_step_ratio
                grid%canhfx(i,j)=canhfx_save(i,j)/fire_time_step_ratio
                grid%canqfx(i,j)=canqfx_save(i,j)/fire_time_step_ratio
            enddo
        enddo


   call print_chsum(0,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe-1,jps,jpe,0,0,0,z_at_w,'z_at_w')
   call print_chsum(0,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe-1,jps,jpe,0,0,0,dz8w,'dz8w')
   call print_chsum(0,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe-1,jps,jpe,0,0,0,rho,'rho')
   call print_chsum(0,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,0,grid%mut,'mu')
   call print_3d_stats(ips,ipe,kps,kpe-1,jps,jpe,ims,ime,kms,kme,jms,jme,rho,'rho')
   call print_3d_stats(ips,ipe,kps,kpe-1,jps,jpe,ims,ime,kms,kme,jms,jme,z_at_w,'z_at_w')
   call print_3d_stats(ips,ipe,kps,kpe-1,jps,jpe,ims,ime,kms,kme,jms,jme,dz8w,'dz8w')

    ! --- add heat and moisture fluxes to tendency variables by postulated decay
    do ij=1,grid%num_tiles
       ! SFIRE works on domain by 1 smaller, in last row&col winds are not set properly
       its = grid%i_start(ij)             ! start atmospheric tile in i
       ite = min(grid%i_end(ij),ide-1)    ! end atmospheric tile in i
       jts = grid%j_start(ij)             ! start atmospheric tile in j
       jte = min(grid%j_end(ij),jde-1)    ! end atmospheric tile in j
       kts=kds
       kte=kde

       call fire_tendency(                 &
            ids,ide-1, kds,kde, jds,jde-1, & ! domain dimensions
            ims,ime, kms,kme, jms,jme,      &
            its,ite, kts,kte, jts,jte,      & ! 
            grid%grnhfx,grid%grnqfx,grid%canhfx,grid%canqfx,        & ! fluxes on atm grid 
            config_flags%fire_ext_grnd,config_flags%fire_ext_crwn,config_flags%fire_crwn_hgt,                &
            grid%ht,z_at_w,dz8w,grid%mut,rho,          &
            grid%rthfrten,grid%rqvfrten)                ! out

     enddo

       ! debug print to compare

       call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,grid%rthfrten,'fire_driver_phys:rthfrten')
       call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,grid%rqvfrten,'fire_driver_phys:rqvfrten')

    call message('sfire_driver_em_step: SFIRE step complete')
            
end subroutine sfire_driver_em_step

double precision function TimeInterval2Sec(time)
! convert ESMF_Timeinterval type to seconds
! has double precision type just in case it is needed in future
! will silently convert to single precition on use.
    TYPE(WRFU_TimeInterval), intent(in) :: time
! local
    integer::rc,S,Sn,Sd
! executable
    call WRFU_TimeIntervalGet(time,S=S,Sd=Sd,Sn=Sn,rc=rc)
    if(rc.ne.WRFU_SUCCESS)call crash('TimeInterval2Sec: WRFU_TimeIntervalGet failed')
    ! print *,'WRFU_TimeIntervalGet returned S=',S,'Sn=',Sn,'Sd=',Sd
    if(Sd.ne.0)then
         TimeInterval2Sec=dble(S)+dble(Sn)/dble(Sd)
    else
         TimeInterval2Sec=dble(S)
    endif
end function TimeInterval2Sec

end module module_fr_sfire_driver_wrf

