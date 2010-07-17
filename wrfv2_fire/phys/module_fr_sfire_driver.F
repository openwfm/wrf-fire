! WRF:MEDIATION_LAYER:FIRE_MODEL

!*** Jan Mandel August 2007 - July 2010 
!*** email: Jan.Mandel@gmail.com

! For support please subscribe to the wrf-fire mailing list at NCAR at
! http://mailman.ucar.edu/mailman/listinfo/wrf-fire
! or go to http://www.openwfm.org/wiki/WRF-Fire_user_support 

! This module is the only entry point from WRF-ARW to the wildland 
! fire model. The call to sfire_driver advances the fire model by 
! one timestep. The fire model inputs the wind and outputs 
! temperature and humidity tendencies. The fire model also inputs a 
! number of constant arrays (fuel data, topography). Additional 
! arguments are model state (for data assimilation) and constant arrays 
! the model gives to WRF for safekeeping because it is not allowed 
! to save anything.

! This code is described in [1]. The fire model is coupled with WRF 
! but the fire code itself is not dependent on WRF in any way other 
! than calls to few WRF utilities from module_fr_sfire_util. This 
! model uses a level set function method for advancing the fireline. 
! It is a reimplementation of an earlier model, which used fireline 
! propagation by tracers and was coupled with the Clark-Hall 
! atmospheric code, described in [2]. For WRF documentation see [3].

! If you use this code, please acknowledge our work by citing [1].
! Thank you.

! Acknowledgements
!
! Contributions to development of the level set 
! method by Mijeong Kim. Contribution to fuel calculation by Volodymyr
! Kondratenko.
!
! The fire physics is adapted from an earlier code by Terry  
! L. Clark, Janice L. Coen, and Don Latham. The coupling with WRF is 
! adapted from a code by Ned Patton for coupling of the earlier fire
! model with WRF. The changes in WRF infrastructure and support of
! refined fire grid was provided by John Michalakes.

! Jonathan D. Beezley has set up and maintained the WRF build and
! execution environment, provided software engineering infrastructure 
! including synchronization with the WRF repository, and was responsibe
! for all aspects of WRF modification.

! Refefences
!
! [1] Jan Mandel, Jonathan D. Beezley, Janice L. Coen, and Minjeong Kim,
! Data Asimilation for Wildland Fires: Ensemble Kalman filters in 
! coupled atmosphere-surface models, IEEE Control Systems Magazine 29,
! Issue 3, June 2009, 47-65. 

! [2] T. L. Clark, J. Coen, and D. Latham, Description of a coupled 
! atmosphere-fire model, Intl. J. Wildland Fire, vol. 13, pp. 49-64, 
! 2004
!
! [3] http://www.mmm.ucar.edu/wrf/OnLineTutorial/Introduction/index.html

#define DEBUG_OUT

module module_fr_sfire_driver

use module_fr_sfire_driver_wrf
use module_fr_sfire_model
use module_fr_sfire_phys
use module_fr_sfire_atm
use module_fr_sfire_util
implicit none

private
public :: sfire_driver_em_init, sfire_driver_em_step


contains

subroutine sfire_driver_em_init (grid , config_flags               & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe)

    ! stub to call sfire_driver_em with irun=0 and omit last 3 args

    USE module_domain
    USE module_configure
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
    ! dummies

    real,dimension(1,1,1)::rho,z_at_w,dz8w

    call message('sfire_driver_em_init: SFIRE initialization start')

    ! get fire mesh dimensions
    CALL get_ijk_from_subgrid (  grid ,                   &
                            ifds,ifde, jfds,jfde,kfds,kfde,                        &
                            ifms,ifme, jfms,jfme,kfms,kfme,                        &
                            ifps,ifpe, jfps,jfpe,kfps,kfpe) 

    call sfire_driver_em ( grid , config_flags               & 
            ,1,2,0                        & ! ifun start, end, test steps
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ,rho,z_at_w,dz8w ) 

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

    USE module_domain
    USE module_configure
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
    ! dummies

    real,dimension(ims:ime, kms:kme, jms:jme)::rho,z_at_w,dz8w

    call message('sfire_driver_em_step: SFIRE step start')

    ! get fire mesh dimensions
    CALL get_ijk_from_subgrid (  grid ,                   &
                            ifds,ifde, jfds,jfde,kfds,kfde,                        &
                            ifms,ifme, jfms,jfme,kfms,kfme,                        &
                            ifps,ifpe, jfps,jfpe,kfps,kfpe) 

    call sfire_driver_em ( grid , config_flags               & 
            ,3,6,fire_test_steps                                &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ,rho,z_at_w,dz8w ) 

    call message('sfire_driver_em_step: SFIRE step complete')

end subroutine sfire_driver_em_step

!
!***
!

subroutine sfire_driver_em ( grid , config_flags                    & 
            ,fire_ifun_start,fire_ifun_end,tsteps                   &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe                                   &
            ,rho,z_at_w,dz8w ) 

!*** purpose: driver from grid structure

! Driver layer modules
    USE module_domain
    USE module_configure
    USE module_driver_constants
    USE module_machine
    USE module_tiles
#ifdef DM_PARALLEL
    USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
    USE module_comm_dm   , ONLY : halo_fire_lfn_sub,halo_fire_longlat_sub,halo_fire_ht_sub   &
                                ,halo_fire_zsf_sub, halo_fire_fuel_sub,halo_fire_wind_a_sub &
                                ,halo_fire_wind_f_sub,halo_fire_tign_sub
#endif

    implicit none
!*** arguments
    TYPE(domain) , TARGET :: grid                             ! state 
    TYPE (grid_config_rec_type) , INTENT(IN)  :: config_flags ! namelist
    integer, intent(in)::     fire_ifun_start,fire_ifun_end,tsteps ! driver cycle controls
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde,                              &
             ims,ime, kms,kme, jms,jme,                              &
             ips,ipe, kps,kpe, jps,jpe,                              &
             ifds,ifde, jfds,jfde,                                   &
             ifms,ifme, jfms,jfme,                                   &
             ifps,ifpe, jfps,jfpe 
    real,intent(in),dimension(ims:ime, kms:kme, jms:jme)::rho,  &! air density  (kg/m^3) 
             z_at_w,dz8w                 ! ????????

!*** local
    INTEGER:: fire_num_ignitions
    integer, parameter::fire_max_ignitions=5
    REAL, DIMENSION(fire_max_ignitions)::  &
        fire_ignition_start_x, &
        fire_ignition_start_y, &
        fire_ignition_end_x, &
        fire_ignition_end_y, &
        fire_ignition_start_time, &
        fire_ignition_end_time, &
        fire_ignition_radius
    integer::fire_ifun,ir,jr,fire_ignition_longlat,istep,itimestep
    logical::need_lfn_update,restart
    !real, dimension(ifms:ifme, jfms:jfme)::uf,vf,lfn_out  
    ! uf vf only do not need to be in the state but we need halo on them
    real, dimension(ifms:ifme, jfms:jfme)::lfn_out  
    real:: corner_ll,corner_ul,corner_ur,corner_lr
    character(len=128)msg
    real:: unit_fxlong ,unit_fxlat
    type(fire_params)::fp


!*** executable

    call print_id

! populate our structures from wrf

    ! pointers to be passed to fire spread formulas
    fp%vx => grid%uf         ! fire winds
    fp%vy => grid%vf         ! fire winds
    fp%zsf => grid%zsf       ! terrain height 
    fp%dzdxf => grid%dzdxf   ! terrain grad 
    fp%dzdyf => grid%dzdyf   ! terrain grad 
    fp%bbb => grid%bbb       ! spread formula coeff 
    fp%betafl => grid%betafl ! spread formula coeff 
    fp%phiwc => grid%phiwc   ! spread formula coeff 
    fp%r_0 => grid%r_0       ! spread formula coeff 
    fp%fgip => grid%fgip     ! spread formula coeff 
    fp%ischap => grid%ischap ! spread formula coeff 
            
    ! get ignition data 
    call fire_ignition_convert (config_flags,fire_max_ignitions,fire_ignition_longlat, &
        fire_ignition_start_x,fire_ignition_start_y,fire_ignition_end_x,fire_ignition_end_y, &
        fire_ignition_radius,fire_ignition_start_time,fire_ignition_end_time,fire_num_ignitions, &
        unit_fxlong,unit_fxlat)

    ! copy configuration flags to sfire internal structures
    call set_flags(config_flags)

    ! refinement r
    ir=grid%sr_x ! refinement ratio
    jr=grid%sr_y
    itimestep=grid%itimestep
    restart=config_flags%restart

    

!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
    write(msg,'(a,i1,a,i1,a,i4)') &
       'sfire_driver_em: ifun from ',fire_ifun_start,' to ',fire_ifun_end,' test steps',tsteps
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
    call message(msg)

    do istep=0,tsteps ! istep >0 is for testing only, exit after the first call
      itimestep = grid%itimestep + istep ! in the first call, do fire_test_steps steps of the fire model

      need_lfn_update=.false.
      do fire_ifun=fire_ifun_start,fire_ifun_end

        ! 1 = initialize run pass 1: interpolate height to zsf=terrain
        ! 2 = initialize run pass 2: set fuel data, terrain gradient
        ! 3 = initialize timestep: interpolate winds, check for ignition
        ! 4 = do one timestep 
        ! 5 = copy timestep output to input
        ! 6 = compute output fluxes

#ifdef DM_PARALLEL
        if(need_lfn_update)then
!           halo exchange on lfn width 2
#include "HALO_FIRE_LFN.inc"
        endif

        if(fire_ifun.eq.1)then
!       halo exchange on topography
#include "HALO_FIRE_LONGLAT.inc"
            if(fire_topo_from_atm.eq.1)then
#include "HALO_FIRE_HT.inc"
            endif 

        elseif(fire_ifun.eq.2)then
!           halo exchange on zsf width 2
#include "HALO_FIRE_ZSF.inc"

        elseif(fire_ifun.eq.3)then
!           halo exchange on atm winds width 1 for interpolation
#include "HALO_FIRE_WIND_A.inc"

        elseif(fire_ifun.eq.4)then
!           halo exchange on fire winds width 2 for a 2-step RK method
#include "HALO_FIRE_WIND_F.inc"

        elseif(fire_ifun.eq.6)then
!           computing fuel_left needs ignition time from neighbors
#include "HALO_FIRE_TIGN.inc"

        endif
#endif
        ! need domain by 1 smaller, in last row.col winds are not set properly
        call sfire_driver_phys ( &
            fire_ifun,need_lfn_update,                  &
            ids,ide-1, kds,kde, jds,jde-1,                          &
            ims,ime, kms,kme, jms,jme,                          &
            ips,min(ipe,ide-1), kps,kpe, jps,min(jpe,jde-1),                          & 
            ifds,ifde-ir, jfds,jfde-jr,                    &
            ifms,ifme, jfms,jfme,                    &
            ifps,min(ifpe,ifde-ir), jfps,min(jfpe,jfde-jr),      &
            ir,jr,                                      & ! atm/fire grid ratio
            grid%num_tiles,                             & ! atm grid tiling
            grid%i_start,min(grid%i_end,ide-1),                    &
            grid%j_start,min(grid%j_end,jde-1),                    &                 
            itimestep,restart,config_flags%fire_fuel_read,config_flags%fire_fuel_cat, &  ! in scalars
            grid%dt,grid%dx,grid%dy,                    &
            grid%u_frame,grid%v_frame,                  &
            unit_fxlong,unit_fxlat,                           & ! coordinates of grid center
            config_flags%fire_ext_grnd,config_flags%fire_ext_crwn,config_flags%fire_crwn_hgt, &
            fire_num_ignitions,                                & 
            fire_ignition_longlat,      &
            fire_ignition_start_x,fire_ignition_start_y, & ! ignition - small arrays
            fire_ignition_end_x,fire_ignition_end_y,     &
            fire_ignition_radius,        &
            fire_ignition_start_time,     &
            fire_ignition_end_time,     &
            grid%u_2,grid%v_2,grid%mut,rho,grid%ht,      & ! in arrays, on atm grid
            z_at_w,dz8w,                                  &
            grid%xlong,grid%xlat,                         & ! coordinates of atm grid centers, for ignition location           
            grid%lfn,grid%tign_g,grid%fuel_frac,          & ! state arrays, fire grid
            grid%fire_area,                               & ! redundant, for display, fire grid
            lfn_out,                                      & ! work - one timestep    
            grid%rthfrten,grid%rqvfrten,                & ! out arrays, atm grid
            grid%grnhfx,grid%grnqfx,grid%canhfx,grid%canqfx, & ! out redundant arrays, atm grid
            grid%fgrnhfx,grid%fgrnqfx,grid%fcanhfx,grid%fcanqfx, & ! out redundant arrays, atm grid
            grid%ros,                                   & ! rate of spread
            grid%fxlong,grid%fxlat,                           &       
            grid%nfuel_cat,                               & ! input, or internal for safekeeping
            grid%fuel_time,                      & 
            fp &
        )

#ifdef DM_PARALLEL
            if(fire_ifun.eq.2)then
!               halo exchange on all fuel data width 2
#include "HALO_FIRE_FUEL.inc"
            endif
#endif

                

      enddo
    enddo
    if(tsteps>0)call crash('sfire_driver_em: test run of uncoupled fire model completed')

end subroutine sfire_driver_em
!
!*******************
!
! module_fr_sfire_driver%%sfire_driver
subroutine sfire_driver_phys (ifun,need_lfn_update,    &
    ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
    ims,ime, kms,kme, jms,jme,                    &
    ips,ipe, kps,kpe, jps,jpe,                    &
    ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
    ifms, ifme, jfms, jfme,                       &
    ifps, ifpe, jfps, jfpe,                       & ! fire patch in - will use smaller
    ir,jr,                                        & ! atm/fire grid ratio
    num_tiles,i_start,i_end,j_start,j_end,        & ! atm grid tiling
    itimestep,restart,ifuelread,nfuel_cat0,dt,dx,dy,      & ! in scalars
    u_frame,v_frame,                              &
    unit_fxlong,unit_fxlat,                       & ! fxlong, fxlat units in m  
    fire_ext_grnd,fire_ext_crwn,fire_crwn_hgt,    &
    num_ignitions,                                & 
    ignition_longlat,                             &
    ignition_start_x,ignition_start_y,            & ! ignition - small arrays
    ignition_end_x,ignition_end_y,                &
    ignition_radius,                              &
    ignition_start_time,                          &
    ignition_end_time,                            &
    u,v,mu,rho,zs,                                & ! in arrays, atm grid
    z_at_w,dz8w,                                  &
    xlong,xlat,                                   &
    lfn,tign,fuel_frac,                           & ! state arrays, fire grid
    fire_area,                                    & ! redundant state, fire grid
    lfn_out,                                & ! fire wind velocities, out level set function    
    rthfrten,rqvfrten,                            & ! out arrays, atm grid
    grnhfx,grnqfx,canhfx,canqfx,                  & ! out redundant arrays, atm grid  
    fgrnhfx,fgrnqfx,fcanhfx,fcanqfx,              & ! out redundant arrays, fire grid
    ros,                                          &
    fxlong,fxlat,                                 & !  
    nfuel_cat,                                    & ! in array, data, fire grid, or constant internal
    fuel_time,                                & ! save constant internal data, fire grid
    fp                                           & ! fire params
    )
USE module_dm, only:wrf_dm_maxval_integer

implicit none

!*** arguments

integer, intent(in)::ifun,                        &
    ids,ide, kds,kde, jds,jde,                    & ! atm domain bounds
    ims,ime, kms,kme, jms,jme,                    & ! atm memory bounds 
    ips,ipe, kps,kpe, jps,jpe,                    & ! atm patch bounds
    ifds, ifde, jfds, jfde,                       & ! fire domain bounds
    ifms, ifme, jfms, jfme,                       & ! fire memory bounds
    ifps, ifpe, jfps, jfpe,                       & ! fire patch bounds
    ir,jr,                                        & ! atm/fire grid refinement ratio
    itimestep,                                    & ! number of this timestep
    ifuelread,                                    & ! how to initialize nfuel_cat:
                                                       ! -1=not at all, done outside 
                                                       ! 0=from nfuel_cat0
                                                       ! 1=from altitude
                                                       ! 2=from file
    nfuel_cat0,                                   & ! fuel category to initialize everything to
    num_tiles                                       ! number of tiles

logical, intent(in)::restart
    

logical, intent(out)::need_lfn_update

integer,dimension(num_tiles),intent(in) :: i_start,i_end,j_start,j_end  ! atm grid tiling

real, intent(in):: &
    dt,                                           & ! time step
    dx,dy,                                        & ! atm grid step
    u_frame,v_frame,                              & ! velocity offset
    unit_fxlong,unit_fxlat,                       & ! fxlong, fxlat units in m  
    fire_crwn_hgt,                                & ! lowest height crown fire heat is released (m)
    fire_ext_grnd,                                & ! extinction depth of ground fire heat (m)
    fire_ext_crwn                                   !  extinction depth of crown fire heat (m)


integer, intent(in):: num_ignitions                 ! number of ignitions, can be 0
real, dimension(num_ignitions), intent(in):: &   
    ignition_start_x,ignition_start_y, &
    ignition_end_x,ignition_end_y,ignition_radius, & ! start, end, radius, time
    ignition_start_time,     &                      !  of ignition lines start
    ignition_end_time                           !  of ignition lines end
integer, intent(in):: ignition_longlat       ! if 1 ignition give as long/lat, otherwise as m from lower left corner

real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::u,v ! wind velocity (m/s) (node based, atm grid) 
real,intent(in),dimension(ims:ime,jms:jme)::mu          ! dry air mass (Pa)  pressure??  (cell based, atm grid)
real,intent(in),dimension(ims:ime, jms:jme)::  zs ! terrain height  
real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::rho, &  ! air density  (kg/m^3) (cell based, atm grid) 
                z_at_w,dz8w                         ! height of some sort??

real, dimension(ims:ime, jms:jme), intent(inout)::xlong, xlat ! inout because of extension at bdry
    
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: &
    nfuel_cat                                       ! fuel data; can be also set inside (cell based, fire grid)

real, intent(inout), dimension(ifms:ifme, jfms:jfme)::     &
    lfn,tign,fuel_frac,                        &     ! state: level function, ign time, fuel left
    lfn_out                                    ! fire wind velocities

real, intent(out), dimension(ifms:ifme, jfms:jfme)::  &
    fire_area                                        ! fraction of each cell burning

real, intent(out), dimension(ims:ime, kms:kme, jms:jme):: &
    rthfrten,rqvfrten                              ! temperature and humidity tendencies (atm grid)

real, intent(out), dimension(ims:ime, jms:jme):: &  ! redundant arrays, for display purposes only (atm grid)
    grnhfx,                                      &  ! heat flux from ground fire (W/m^2) 
    grnqfx,                                      &  ! moisture flux from ground fire (W/m^2) 
    canhfx,                                      &  ! heat flux from crown fire (W/m^2) 
    canqfx                                         ! moisture flux from crown fire (W/m^2) 

real, intent(out), dimension(ifms:ifme, jfms:jfme):: &  ! redundant arrays, for display only, fire grid
    fgrnhfx,                                      &  ! heat flux from ground fire (W/m^2) 
    fgrnqfx,                                      &  ! moisture flux from ground fire (W/m^2) 
    fcanhfx,                                      &  ! heat flux from crown fire (W/m^2) 
    fcanqfx,                                      &   ! moisture flux from crown fire (W/m^2) 
    ros                                             ! fire rate of spread (m/s)

!  ***** data (constant in time) *****

real, dimension(ifms:ifme, jfms:jfme), intent(inout)::fxlong,fxlat ! fire mesh coordinates
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time   ! fire params arrays

type(fire_params),intent(inout)::fp
    
!*** local
real :: dxf,dyf,time_start,latm
integer :: its,ite,jts,jte,kts,kte, &            ! tile
    ij,i,j,k,id,pid,kpe1, &
    ifts,ifte,jfts,jfte                          ! fire tile
character(len=128)::msg
character(len=3)::kk
integer::ignitions_done_tile(num_tiles),ignited_tile(num_ignitions,num_tiles)
integer::ignitions_done,ignited_patch(num_ignitions),idex,jdex


!*** executable

! time - assume dt does not change
time_start = itimestep * dt

! fire mesh step
dxf=dx/ir
dyf=dy/jr



!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
write(msg,'(a,2f15.6)')'atmosphere mesh step:',dx,dy
call message(msg)
write(msg,'(a,2f15.6)')'fire mesh step:      ',dxf,dyf
call message(msg)
write(msg,7001)'atm domain      ','ids',ids,ide,jds,jde
call message(msg)                    
write(msg,7001)'atm memory      ','ims',ims,ime,jms,jme
call message(msg)                    
write(msg,7001)'atm patch       ','ips',ips,ipe,jps,jpe
call message(msg)                    
write(msg,7001)'fire domain     ','ifds',ifds,ifde,jfds,jfde
call message(msg)                    
write(msg,7001)'fire memory     ','ifms',ifms,ifme,jfms,jfme
call message(msg)                    
write(msg,7001)'fire patch      ','ifps',ifps,ifpe,jfps,jfpe
call message(msg)                    
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)

! check mesh dimensions
call check_fmesh(ids,ide,ifds,ifde,ir,'id')           ! check if atm and fire grids line up
call check_fmesh(jds,jde,jfds,jfde,jr,'jd')
call check_fmesh(ips,ipe,ifps,ifpe,ir,'ip')
call check_fmesh(jps,jpe,jfps,jfpe,jr,'jp')
call check_mesh_2dim(ips,ipe,jps,jpe,ims,ime,jms,jme)        ! check if atm patch fits in atm array
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme) ! check if fire patch fits in fire array
call check_mesh_2dim(ips,ipe,jps,jpe,ids,ide,jds,jde)        ! check if atm patch fits in atm domain
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifds,ifde,jfds,jfde) ! check if fire patch fits in fire domain

!$OMP SINGLE
! init rest of fuel tables with derived constants
if(ifun.eq.1) then
   call init_fuel_cats  ! common for all threads
endif
!$OMP END SINGLE


pid=0
if(itimestep.le.10.or.mod(itimestep,10).eq.0)pid=itimestep ! print 1-10 then every 10th
!pid=itimestep


! fake atm tile bounds
kts=kps
kte=kpe

! set up fire tiles & interpolate to fire grid
!$OMP PARALLEL DO PRIVATE(ij,its,ite,jts,jte,ifts,ifte,jfts,jfte,msg,id) &
!$OMP SCHEDULE(STATIC)
do ij=1,num_tiles

    id=0  ! do not print/write anything
    if(itimestep.le.10.or.mod(itimestep,10).eq.0)id=itimestep+ij*10000

    ignitions_done_tile(ij)=0

    ! set up tile bounds    
    its = i_start(ij)  ! start atmospheric tile in i
    ite = i_end(ij)    ! end atmospheric tile in i
    jts = j_start(ij)  ! start atmospheric tile in j
    jte = j_end(ij)    ! end atmospheric tile in j
    ifts= (its-ids)*ir+ifds       ! start fire tile in i
    ifte= (ite-ids+1)*ir+ifds-1   ! end fire tile in i
    jfts= (jts-jds)*jr+jfds       ! start fire tile in j
    jfte= (jte-jds+1)*jr+jfds-1   ! end fire tile in j
        
!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
    write(msg,'(3(a,i3,1x))')'tile=',ij,' id=',id,' ifun=',ifun
    call message(msg)
    write(msg,7001)'atm tile   ','its',its,ite,jts,jte
    call message(msg)                   
    write(msg,7001)'fire tile  ','ifts',ifts,ifte,jfts,jfte
    call message(msg)                    
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)

    ! check the tiles
    call check_mesh_2dim(its,ite,jts,jte,ips,ipe,jps,jpe)                 ! check if atm tile fits in atm patch
    call check_mesh_2dim(ifts,ifte,jfts,jfte,ifps,ifpe,jfps,jfpe)         ! check if fire tile fits in fire patch
    call check_mesh_2dim(ifts-2,ifte+2,jfts-2,jfte+2,ifms,ifme,jfms,jfme)! check if fire node tile fits in memory


!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
    write(msg,'(a,i6,a,2(f15.6,a))')'time step',itimestep,' at',time_start,' duration',dt,'s'
    call message(msg)
    7001 format(a,' dimensions ',a4,':',i6,' to ',i6,' by ',i6,' to ',i6)
    write(msg,'(a,2i9)')'refinement ratio:',ir,jr
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)

    if(ifun.eq.1)then   ! set terrain

      if(restart)then
          
          call message('restart - topo initialization skipped')

      else

        call print_2d_stats(ips,ipe,jps,jpe,ims,ime,jms,jme,zs,'driver:zs')
    
        ! interpolate terrain height
        
        if(fire_topo_from_atm.eq.1)then
            call interpolate_z2fire(id,                 & ! for debug output, <= 0 no output
                ids,ide,  jds,jde,                    & ! atm grid dimensions
                ims,ime,  jms,jme,                    &
                ips,ipe,jps,jpe,                              &
                its,ite,jts,jte,                              &
                ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
                ifms, ifme, jfms, jfme,                       &
                ifts,ifte,jfts,jfte,                          &
                ir,jr,                                        & ! atm/fire grid ratio
                zs,                                       & ! atm grid arrays in
                fp%zsf)                                      ! fire grid arrays out
        else
!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
           write(msg,'(a,i3,a)')'fire_topo_from_atm=',fire_topo_from_atm,' assuming ZSF set, interpolation skipped'
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
        endif

        if(ignition_longlat .eq.0)then
            ! set ideal fire mesh coordinates - used for ignition only
            ! do not forget to set unit_fxlong, unit_fxlat outside of parallel loop
            call set_ideal_coord( dxf,dyf, &
                ifds,ifde,jfds,jfde,  &
                ifms,ifme,jfms,jfme,  &
                ifts,ifte,jfts,jfte,  &
                fxlong,fxlat          )
        else
            ! assume halo xlong xlat
            ! interpolate nodal coordinates

#ifdef DEBUG_OUT
         call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,xlat,'xlat',id)
         call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,xlong,'xlong',id)
#endif
        call interpolate_z2fire(id,                 & ! for debug output, <= 0 no output
            ids,ide,  jds,jde,                    & ! atm grid dimensions
            ims,ime,  jms,jme,                    &
            ips,ipe,jps,jpe,                              &
            its,ite,jts,jte,                              &
            ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
            ifms, ifme, jfms, jfme,                       &
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        & ! atm/fire grid ratio
            xlat,                                       & ! atm grid arrays in
            fxlat)                                      ! fire grid arrays out

        call interpolate_z2fire(id,                 & ! for debug output, <= 0 no output
            ids,ide,  jds,jde,                    & ! atm grid dimensions
            ims,ime,  jms,jme,                    &
            ips,ipe,jps,jpe,                              &
            its,ite,jts,jte,                              &
            ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
            ifms, ifme, jfms, jfme,                       &
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        & ! atm/fire grid ratio
            xlong,                                       & ! atm grid arrays in
            fxlong)                                      ! fire grid arrays out

        endif

     endif

    elseif(ifun.eq.2)then  
               
        ! after the loop where zsf created exited and all synced
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fp%zsf,'driver_phys:zsf')        

    elseif(ifun.eq.3)then  ! interpolate winds to the fire grid
    
        call interpolate_atm2fire(id,                     & ! flag for debug output
            ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
            ims,ime, kms,kme, jms,jme,                    &
            ips,ipe,jps,jpe,                              &
            its,ite,jts,jte,                              &                    
            ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
            ifms, ifme, jfms, jfme,                       &
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        & ! atm/fire grid ratio
            u_frame, v_frame,                             & ! velocity frame correction
            u,v,                                       & ! atm grid arrays in
            fp%vx,fp%vy)                                      ! fire grid arrays out
    
    endif

!   the following executes in any case

    call sfire_model (id,ifun,restart,need_lfn_update,  &
        num_ignitions,                          & ! switches
        ifuelread,nfuel_cat0,                   & ! initialize fuel categories
        ifds,ifde,jfds,jfde,                    & ! fire domain dims
        ifms,ifme,jfms,jfme,                    & ! fire memory dims
        ifps,ifpe,jfps,jfpe,                    &
        ifts,ifte,jfts,jfte,                    & ! fire patch dims
        time_start,dt,                          & ! time and increment
        dxf,dyf,                                & ! fire mesh spacing
        ignition_start_x,ignition_start_y,      & ! ignition - small arrays
        ignition_end_x,ignition_end_y,          &
        ignition_radius,                        &
        ignition_start_time,                    &
        ignition_end_time,                      &
        ignitions_done_tile(ij),ignited_tile(1,ij),  &
        fxlong,fxlat,unit_fxlong,unit_fxlat,      & ! fire mesh coordinates
        lfn,lfn_out,tign,fuel_frac,                     & ! state: level function, ign time, fuel left
        fire_area,                              & ! output: fraction of cell burning
        fgrnhfx,fgrnqfx,                        & ! output: heat fluxes
        ros,                                    & ! output: rate of spread for display
        nfuel_cat,                              & ! fuel data per point 
        fuel_time,                              & ! save derived internal data
        fp                                      & ! fire coefficients
    )
    
    if(ifun.eq.6)then ! heat fluxes into the atmosphere    
    
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnhfx,'fire_driver:fgrnhfx')
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnqfx,'fire_driver:fgrnqfx')
    
        ! sum the fluxes over atm cells
        call sum_2d_cells(        &
            ifms,ifme,jfms,jfme,  &
            ifts,ifte,jfts,jfte,  &
            fgrnhfx,              &
            ims, ime, jms, jme,   &
            its,ite,jts,jte,      &
            grnhfx)
!comment out the next call to get results as before commit 55fd92051196b796891b60cb7ec1c4bdb8800078
        call sum_2d_cells(        &
            ifms,ifme,jfms,jfme,  &
            ifts,ifte,jfts,jfte,  &
            fgrnqfx,              &
            ims, ime, jms, jme,   &
            its,ite,jts,jte,      &
            grnqfx)

!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
        write(msg,'(a,f6.3)')'fire-atmosphere feedback scaling ',fire_atm_feedback
!$OMP end CRITICAL(SFIRE_DRIVER_CRIT)
	call message(msg)
        do j=jts,jte
            do i=its,ite
                ! scale ground fluxes to get the averages
                grnhfx(i,j)=fire_atm_feedback*grnhfx(i,j)/(ir*jr)
                grnqfx(i,j)=fire_atm_feedback*grnqfx(i,j)/(ir*jr)
                ! we do not have canopy fluxes yet...
                canhfx(i,j)=0
                canqfx(i,j)=0
            enddo
        enddo

        do j=jts,jte
            do k=kts,min(kte+1,kde)
               do i=its,ite
                   rthfrten(i,k,j)=0.
                   rqvfrten(i,k,j)=0.
               enddo
            enddo
        enddo


        ! --- add heat and moisture fluxes to tendency variables by postulated decay

       call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnhfx,'fire_driver:grnhfx')
       call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnqfx,'fire_driver:grnqfx')

       call fire_tendency(                 &
            ids,ide, kds,kde, jds,jde,      & ! dimensions
            ims,ime, kms,kme, jms,jme,      &
            its,ite, kts,kte, jts,jte,      & ! 
            grnhfx,grnqfx,canhfx,canqfx,        & ! fluxes on atm grid 
            fire_ext_grnd,fire_ext_crwn,fire_crwn_hgt,                &
            zs,z_at_w,dz8w,mu,rho,          &
            rthfrten,rqvfrten)                ! out

       ! debug print to compare

       call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,rthfrten,'fire_driver_phys:rthfrten')
       call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,rqvfrten,'fire_driver_phys:rqvfrten')

            
    endif ! ifun=6

enddo ! tiles
!$OMP END PARALLEL DO

if (ifun.eq.3)then
    ignitions_done=ignitions_done_tile(1) ! all should be the same
    do i=1,ignitions_done
!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
        write(msg,'(2(a,i4,1x))')'sfire_driver_phys: checking ignition ',i,' of ',ignitions_done
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
        call message(msg)
        ignited_patch(i)=0
        do ij=1,num_tiles
            ignited_patch(i)=ignited_patch(i)+ignited_tile(i,ij)
        enddo
#ifdef DM_PARALLEL
        call message('sfire_driver_phys: checking ignitions, collect counts')
        call wrf_dm_maxval_integer(ignited_patch(i),idex,jdex)
        call message('sfire_driver_phys: collected')
#endif
        if(ignited_patch(i).eq.0)then
            call crash('sfire_driver_phys: Ignition failed, no nodes ignited. Bad coordinates?')
        endif
    enddo
endif

if(ifun.eq.1)then
#ifdef DEBUG_OUT
    if(pid.ne.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,zs,'zs',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%zsf,'zsf',pid)
    endif
#endif
elseif(ifun.eq.3)then
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m3(ips,ipe+1,kds,kds+1,jps,jpe+1,ims,ime,kms,kme,jms,jme,u,'u',pid)
        call write_array_m3(ips,ipe+1,kds,kds+1,jps,jpe+1,ims,ime,kms,kme,jms,jme,v,'v',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%vx,'uf',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%vy,'vf',pid)
    endif
#endif
elseif(ifun.eq.5)then
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,lfn,'lfn',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,tign,'tign',pid)
    endif
#endif
elseif(ifun.eq.6)then
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnhfx,'grnhfx',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnqfx,'grnqfx',pid)
        call write_array_m3(ips,ipe,kps,kpe,jps,jpe,ims,ime,kms,kme,jms,jme,rthfrten,'rthfrten',pid)
        call write_array_m3(ips,ipe,kps,kpe,jps,jpe,ims,ime,kms,kme,jms,jme,rqvfrten,'rqvfrten',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fuel_frac,'fuel_frac',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnhfx,'fgrnhfx',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnqfx,'fgrnqfx',pid)
    endif
#endif
    !call print_2d_stats(ips,ipe,jps,jpe,ims,ime,jms,jme,mu,'driver:mu')
    !call print_3d_stats(ips,ipe,kps,kpe,jps,jpe,ims,ime,kms,kme,jms,jme,rho,'driver:rho')
    kpe1=min(kps+1,kpe)
    !kpe1=kps-1
    do k=kts,min(kte,kts+3)
!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
        write(kk,'(i2)')k
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
        call print_3d_stats(ips,ipe,k,k,jps,jpe,ims,ime,kms,kme,jms,jme,rthfrten,kk//'driver_phys:rthfrten')
        call print_3d_stats(ips,ipe,k,k,jps,jpe,ims,ime,kms,kme,jms,jme,rqvfrten,kk//'driver_phys:rqvfrten')
    enddo
endif

end subroutine sfire_driver_phys
end module module_fr_sfire_driver

