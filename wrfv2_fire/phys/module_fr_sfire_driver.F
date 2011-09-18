
! SFIRE - Spread fire model in WRF-Fire
!
!*** Jan Mandel August 2007 - March 2011 
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

! This code as of mid-2011 is described in [1]. 

! If you use this code, please acknowledge our work by citing [1].
! Thank you.

! Acknowledgements
!
! The fire physics code is adapted from the CAWFE code [2].
! The interface with is adapted from a code by Ned Patton  
! coupling a port of the CAWFE fire module to WRF [3].
! Support of refined fire grids was provided by John Michalakes.
! Jonathan D. Beezley has set up and maintained the WRF build and
! execution environment, provided software engineering infrastructure 
! including synchronization with the WRF repository, and was responsibe
! for all aspects of WRF modification. UCD students Minjeong Kim and
! Volodymyr Kondratenko have contributed to the implementation of the
! fire propagation by the level set method.

! Refefences
!
! [1] Jan Mandel, Jonathan D. Beezley, and Adam K. Kochanski, "Coupled atmosphere-wildland
! fire modeling with WRF 3.3 and SFIRE 2011, Geoscientific Model Development (GMD) 4, 
! 591-610, 2011. doi:10.5194/gmd-4-591-2011
!
! [3] T. L. Clark, J. Coen, and D. Latham, Description of a coupled 
! atmosphere-fire model, Intl. J. Wildland Fire, vol. 13, pp. 49-64, 
! 2004
!
! [4] Edward G. Patton and Janice L. Coen, WRF-Fire: A Coupled 
! Atmosphere-Fire Module for WRF, Preprints of Joint MM5/Weather 
! Research and Forecasting Model Users' Workshop, Boulder, CO, 
! June 22-25, 2004, pp. 221-223, NCAR
!

module module_fr_sfire_driver
! use this module for standalone call, you only need to provide some mock-up wrf modules  

use module_fr_sfire_model
use module_fr_sfire_phys
use module_fr_sfire_atm
use module_fr_sfire_util
use module_fr_sfire_core, only: ignition_line_type

USE module_domain, only: domain
USE module_configure, only: grid_config_rec_type
use module_model_constants, only: reradius,    & ! 1/earth radiusw
                                  pi2            ! 2*pi

implicit none


private
public sfire_driver_em,use_atm_vars

logical:: use_atm_vars=.true.   !  interpolate wind from atm mesh and average output fluxes back

contains

#define DEBUG_OUT
subroutine sfire_driver_em ( grid , config_flags                    & 
            ,time_step_start,dt                                     &
            ,fire_ifun_start,fire_ifun_end,tsteps                   &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe )

!*** purpose: driver from grid structure

! Driver layer modules
#ifdef DM_PARALLEL
    USE module_dm        , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks
    USE module_comm_dm 
#endif

    implicit none
!*** arguments
    TYPE(domain) , TARGET :: grid                             ! state 
    TYPE (grid_config_rec_type) , INTENT(IN)  :: config_flags ! namelist
    real, intent(in):: time_step_start, dt
    integer, intent(in)::     fire_ifun_start,fire_ifun_end,tsteps ! driver cycle controls
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde,                              &
             ims,ime, kms,kme, jms,jme,                              &
             ips,ipe, kps,kpe, jps,jpe,                              &
             ifds,ifde, jfds,jfde,                                   &
             ifms,ifme, jfms,jfme,                                   &
             ifps,ifpe, jfps,jfpe 

!*** local
    INTEGER:: fire_num_ignitions
    integer, parameter::fire_max_ignitions=5
    TYPE(ignition_line_type), DIMENSION(fire_max_ignitions):: ignition_line
    integer::fire_ifun,ir,jr,fire_ignition_longlat,istep,itimestep
    logical::restart
    real, dimension(ifms:ifme, jfms:jfme)::lfn_out  
    real:: corner_ll,corner_ul,corner_ur,corner_lr
    character(len=128)msg
    real:: unit_fxlong ,unit_fxlat
    type(fire_params)::fp


!*** executable

    call time_start
    if(fire_ifun_start.le.1)call print_id  ! print id only once, during initialization

! populate our structures from wrf

    ! pointers to be passed to fire spread formulas
    fp%vx => grid%uf         ! fire winds
    fp%vy => grid%vf         ! fire winds
    fp%zsf => grid%zsf       ! terrain height 
    fp%dzdxf => grid%dzdxf   ! terrain grad 
    fp%dzdyf => grid%dzdyf   ! terrain grad 
    fp%bbb => grid%bbb       ! spread formula coeff 
    fp%phisc => grid%phisc   ! spread formula coeff 
    fp%phiwc => grid%phiwc   ! spread formula coeff 
    fp%r_0 => grid%r_0       ! spread formula coeff 
    fp%fgip => grid%fgip     ! spread formula coeff 
    fp%ischap => grid%ischap ! spread formula coeff 
    fp%fuel_time => grid%fuel_time ! time for fuel to burn to 1/e
            
    ! get ignition data 
    call fire_ignition_convert (config_flags,fire_max_ignitions,fire_ignition_longlat, &
        ignition_line,fire_num_ignitions,unit_fxlong,unit_fxlat)

    ! copy configuration flags to sfire internal structures
    call set_flags(config_flags)

    ! refinement r
    ir=grid%sr_x ! refinement ratio
    jr=grid%sr_y
    itimestep=grid%itimestep
    restart=config_flags%restart .or. config_flags%cycling

    

!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
    write(msg,'(a,i1,a,i1,a,l1)') &
       'sfire_driver_em: ifun from ',fire_ifun_start,' to ',fire_ifun_end,' restart=',restart
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
    call message(msg)

    do istep=0,tsteps ! istep >0 is for testing only, exit after the first call
      itimestep = grid%itimestep + istep ! in the first call, do fire_test_steps steps of the fire model

      do fire_ifun=fire_ifun_start,fire_ifun_end

        ! 1 = initialize run pass 1: interpolate height to zsf=terrain
        ! 2 = initialize run pass 2: set fuel data, terrain gradient
        ! 3 = initialize timestep: interpolate winds, check for ignition
        ! 4 = do one timestep 
        ! 5 = copy timestep output to input
        ! 6 = compute output fluxes

#ifdef DM_PARALLEL

        if(fire_ifun.eq.1)then
!       halo exchange on topography
#include "HALO_FIRE_LONGLAT.inc"
!!            if(fire_topo_from_atm.eq.1)then
!!#include "HALO_FIRE_HT.inc"
!!            endif 
! base geopotential and roughness
#include "HALO_FIRE_PHB.inc"
#include "HALO_FIRE_Z0.inc"

        elseif(fire_ifun.eq.2)then
!           halo exchange on zsf width 2
#include "HALO_FIRE_ZSF.inc"

        elseif(fire_ifun.eq.3)then
!           halo exchange on atm winds and geopotential, width 1 for interpolation
#include "HALO_FIRE_WIND_A.inc"
#include "HALO_FIRE_PH.inc"

        elseif(fire_ifun.eq.4)then
!           halo exchange on fire winds width 2 for a 2-step RK method
#include "HALO_FIRE_WIND_F.inc"

        elseif(fire_ifun.eq.6)then
!           computing fuel_left needs ignition time from neighbors
#include "HALO_FIRE_TIGN.inc"
            call message('halo exchange on lfn width 2')
#include "HALO_FIRE_LFN.inc"

        endif
#endif
        ! print *,'dt: ',dt,grid%dt,' diff ', dt-grid%dt
        ! need domain by 1 smaller, in last row.col winds are not set properly
        call sfire_driver_phys ( &
            fire_ifun,                  &
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
            time_step_start,dt,grid%dx,grid%dy,                    &
            grid%u_frame,grid%v_frame,                  &
            unit_fxlong,unit_fxlat,                           & ! coordinates of grid center
            config_flags%fire_ext_grnd,config_flags%fire_ext_crwn,config_flags%fire_crwn_hgt, &
            fire_num_ignitions,                                & 
            fire_ignition_longlat,      &
            ignition_line,              &
            grid%u_2,grid%v_2,           &          ! atm arrays in
            grid%ph_2,grid%phb,               & ! geopotential
            grid%z0,                        & ! roughness height
            grid%ht,                        &                         ! terrain height
            grid%xlong,grid%xlat,                         & ! coordinates of atm grid centers, for ignition location           
            grid%lfn,grid%tign_g,grid%fuel_frac,          & ! state arrays, fire grid
            grid%fire_area,                               & ! redundant, for display, fire grid
            lfn_out,                                      & ! work - one timestep    
            grid%avg_fuel_frac,                           & ! out redundant arrays, atm grid
            grid%grnhfx,grid%grnqfx,grid%canhfx,grid%canqfx, & ! out redundant arrays, atm grid
            grid%uah,grid%vah,                          &
            grid%fgrnhfx,grid%fgrnqfx,grid%fcanhfx,grid%fcanqfx, & ! out redundant arrays, atm grid
            grid%ros,grid%ros0,grid%rosx,grid%rosy, & ! rate of spread diagnostic variables
            grid%fintens,grid%fintens2,grid%fdintens,grid%fdintens2,  &
            grid%fxlong,grid%fxlat,                           &       
            grid%nfuel_cat,                               & ! input, or internal for safekeeping
            grid%fuel_time,                      & 
            grid%fz0, grid%fwh,                    &
            fp &
        )

#ifdef DM_PARALLEL
            if(fire_ifun.eq.2)then
!               halo exchange on all fuel data width 2
#include "HALO_FIRE_FUEL.inc"
!           fire state was initialized
            call message('halo exchange on lfn width 2')
#include "HALO_FIRE_LFN.inc"
            endif
#endif

                

      enddo
    enddo
    if(tsteps>0)call crash('sfire_driver_em: test run of uncoupled fire model completed')
    call time_end('sfire')

end subroutine sfire_driver_em

!
!*******************
!

! module_fr_sfire_driver%%sfire_driver
subroutine sfire_driver_phys (ifun,    &
    ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
    ims,ime, kms,kme, jms,jme,                    &
    ips,ipe, kps,kpe, jps,jpe,                    &
    ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
    ifms, ifme, jfms, jfme,                       &
    ifps, ifpe, jfps, jfpe,                       & ! fire patch in - will use smaller
    ir,jr,                                        & ! atm/fire grid ratio
    num_tiles,i_start,i_end,j_start,j_end,        & ! atm grid tiling
    itimestep,restart,ifuelread,nfuel_cat0,       & ! in scalars
    time_step_start,dt,dx,dy,                     & ! in scalars
    u_frame,v_frame,                              &
    unit_fxlong,unit_fxlat,                       & ! fxlong, fxlat units in m  
    fire_ext_grnd,fire_ext_crwn,fire_crwn_hgt,    &
    num_ignitions,                                & 
    ignition_longlat,                             &
    ignition_line,                                &
    u,v,                                       & ! in arrays, atm grid
    ph,phb,                                       &
    z0,zs,                                        & 
    xlong,xlat,                                   &
    lfn,tign,fuel_frac,                           & ! state arrays, fire grid
    fire_area,                                    & ! redundant state, fire grid
    lfn_out,                                      & ! out level set function    
    avg_fuel_frac,                                &
    grnhfx,grnqfx,canhfx,canqfx,                  & ! out redundant arrays, atm grid  
    uah,vah,                                      & ! out atm grid
    fgrnhfx,fgrnqfx,fcanhfx,fcanqfx,              & ! out redundant arrays, fire grid
    ros,ros0,rosx,rosy,                           & ! fire spread diagnostic variables
    fintens,fintens2,fdintens,fdintens2,          &
    fxlong,fxlat,                                 & !  
    nfuel_cat,                                    & ! in array, data, fire grid, or constant internal
    fuel_time,                                & ! save constant internal data, fire grid
    fz0,fwh,                                      &
    fp                                           & ! fire params
    )
USE module_dm, only:wrf_dm_maxval

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
    


integer,dimension(num_tiles),intent(in) :: i_start,i_end,j_start,j_end  ! atm grid tiling

real, intent(in):: &
    time_step_start,                              & ! time step start
    dt,                                           & ! time step length
    dx,dy,                                        & ! atm grid step
    u_frame,v_frame,                              & ! velocity offset
    unit_fxlong,unit_fxlat,                       & ! fxlong, fxlat units in m  
    fire_crwn_hgt,                                & ! lowest height crown fire heat is released (m)
    fire_ext_grnd,                                & ! extinction depth of ground fire heat (m)
    fire_ext_crwn                                   ! and for the canopy (m) 


integer, intent(in):: num_ignitions                 ! number of ignitions, can be 0
integer, intent(in):: ignition_longlat       ! if 1 ignition give as long/lat, otherwise as m from lower left corner
TYPE (ignition_line_type), DIMENSION(num_ignitions), intent(out):: ignition_line

real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::u,v, & ! wind velocity (m/s) (staggered atm grid) 
                              ph, phb                      ! geopotential (w-points atm grid)
real,intent(in),dimension(ims:ime, jms:jme)::   z0, &    ! roughness height
                                                zs       ! terrain height  
real,intent(out),dimension(ims:ime,jms:jme)::&
    uah,                                           & ! atm wind at fire_wind_height, diagnostics
    vah                                              ! atm wind at fire_wind_height, diagnostics

real, dimension(ims:ime, jms:jme), intent(inout)::xlong, xlat ! inout because of extension at bdry
    
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: & ! fuel data; can be also set inside (cell based, fire grid)
    fz0,fwh,                                          &  
    nfuel_cat                                      

real, intent(inout), dimension(ifms:ifme, jfms:jfme)::     &
    lfn,tign,fuel_frac,                        &     ! state: level function, ign time, fuel left
    lfn_out                                    ! fire wind velocities

real, intent(out), dimension(ifms:ifme, jfms:jfme)::  &
    fire_area                                        ! fraction of each cell burning

real, intent(out), dimension(ims:ime, jms:jme):: &  ! redundant arrays, for display purposes only (atm grid)
    avg_fuel_frac,                               &  ! average fuel fraction
    grnhfx,                                      &  ! heat flux from ground fire (W/m^2) 
    grnqfx,                                      &  ! moisture flux from ground fire (W/m^2) 
    canhfx,                                      &  ! heat flux from crown fire (W/m^2) 
    canqfx                                         ! moisture flux from crown fire (W/m^2) 

real, intent(out), dimension(ifms:ifme, jfms:jfme):: &  ! redundant arrays, for display only, fire grid
    fgrnhfx,                                      &  ! heat flux from ground fire (W/m^2) 
    fgrnqfx,                                      &  ! moisture flux from ground fire (W/m^2) 
    fcanhfx,                                      &  ! heat flux from crown fire (W/m^2) 
    fcanqfx,                                      &  ! moisture flux from crown fire (W/m^2) 
    ros,                                          &  ! fire rate of spread normal to the fireline (m/s)
    ros0,rosx,rosy,                               &  ! fire spread vector (m/s)
    fintens,fintens2,fdintens,fdintens2              ! fireline intensities

!  ***** data (constant in time) *****

real, dimension(ifms:ifme, jfms:jfme), intent(inout)::fxlong,fxlat ! fire mesh coordinates
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time   ! fire params arrays

type(fire_params),intent(inout)::fp
    
!*** local
real :: dxf,dyf,time_start,latm, s
integer :: its,ite,jts,jte,kts,kte, &            ! tile
    ij,i,j,k,id,pid,ipe1,jpe1,ite1,jte1, &
    ii,jj,                                 &
    ifts,ifte,jfts,jfte                          ! fire tile
character(len=128)::msg
character(len=3)::kk
integer::ignitions_done_tile(num_tiles),ignited_tile(num_ignitions,num_tiles)
integer::ignitions_done,ignited_patch(num_ignitions),idex,jdex


!*** executable

! time - assume dt does not change
! time_start = (itimestep-1) * dt     ! timestep 1 starts at 0
! print *,'time_start: ',time_start,time_step_start,' diff ', time_start-time_step_start
time_start = time_step_start ! use the time passed from wrf

! fire mesh step
dxf=dx/ir
dyf=dy/jr


!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
write(msg,'(a,i5)')'sfire_driver_phys stage ',ifun
call message(msg)
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
if(fire_print_file.gt.0)then
    if(itimestep.le.fire_print_file.or.mod(itimestep,fire_print_file).eq.0)pid=itimestep ! print 1-fire_print_file then every fire_print_file-th
endif

if(ifun.eq.3)then
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,1,0,0,u,'u')
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,0,0,1,v,'v')
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,0,1,0,ph,'ph')
endif

! fake atm tile bounds
kts=kps
kte=kpe

! staggered atm patch bounds
ipe1=ifval(ipe.eq.ide,ipe+1,ipe)
jpe1=ifval(jpe.eq.jde,jpe+1,jpe)

! set up fire tiles & interpolate to fire grid
!$OMP PARALLEL DO PRIVATE(ij,its,ite,jts,jte,ite1,jte1,ifts,ifte,jfts,jfte,msg,id) &
!$OMP SCHEDULE(STATIC)
do ij=1,num_tiles

    id = ifval(pid.ne.0,pid+ij*10000,0) ! for print

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
        
! staggered atm tile bounds
    ite1=ifval(ite.eq.ide,ite+1,ite)
    jte1=ifval(jte.eq.jde,jte+1,jte)

!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
    write(msg,'(a,i3,1x,a,i7,1x,a,i3)')'tile=',ij,' id=',id,' ifun=',ifun
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
!
!        call print_2d_stats(ips,ipe,jps,jpe,ims,ime,jms,jme,zs,'driver:zs')
!    
!        ! interpolate terrain height
!        if(fire_topo_from_atm.eq.1)then
!            call interpolate_z2fire(id,                 & ! for debug output, <= 0 no output
!                ids,ide,  jds,jde,                    & ! atm grid dimensions
!                ims,ime,  jms,jme,                    &
!                ips,ipe,jps,jpe,                              &
!                its,ite,jts,jte,                              &
!                ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
!                ifms, ifme, jfms, jfme,                       &
!                ifts,ifte,jfts,jfte,                          &
!                ir,jr,                                        & ! atm/fire grid ratio
!                zs,                                       & ! atm grid arrays in
!                fp%zsf)                                      ! fire grid arrays out
!        else
!!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
!           write(msg,'(a,i3,a)')'fire_topo_from_atm=',fire_topo_from_atm,' assuming ZSF set, interpolation skipped'
!!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
!        endif

        if(ignition_longlat .eq.0)then
            ! set ideal fire mesh coordinates - used for ignition only
            ! do not forget to set unit_fxlong, unit_fxlat outside of parallel loop
            !call set_ideal_coord( dxf,dyf, &
            !    ifds,ifde,jfds,jfde,  &
            !    ifms,ifme,jfms,jfme,  &
            !    ifts,ifte,jfts,jfte,  &
            !    fxlong,fxlat          )
            !call set_ideal_coord( dx,dy, &
            !    ids,ide,jds,jde,  &
            !    ims,ime,jms,jme,  &
            !    its,ite,jts,jte,  &
            !    xlong,xlat          )
        elseif(use_atm_vars)then
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
 
      if(use_atm_vars)then                                  
     
        call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,z0,'z0',id)
        call write_array_m3(its,ite1,kts,kde-1,jts,jte,ims,ime,kms,kme,jms,jme,u,'u_2',id)
        call write_array_m3(its,ite,kts,kde-1,jts,jte1,ims,ime,kms,kme,jms,jme,v,'v_2',id)
        call write_array_m3(its,ite,kts,kde,jts,jte,ims,ime,kms,kme,jms,jme,ph,'ph_2',id)
        call write_array_m3(its,ite,kts,kde,jts,jte,ims,ime,kms,kme,jms,jme,phb,'phb',id)
        
        if(fire_wind_log_interp.eq.4)then
          call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,z0,'driver_phys:z0')
          call interpolate_atm2fire(id,                     & ! flag for debug output
            ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
            ims,ime, kms,kme, jms,jme,                    &
            ips,ipe, jps,jpe,                             &
            its,ite,jts,jte,                              &                    
            ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
            ifms, ifme, jfms, jfme,                       &
            ifps, ifpe, jfps, jfpe,                       & ! fire patch bounds
            ifts, ifte, jfts, jfte,                       &
            ir,jr,                                        & ! atm/fire grid ratio
            u_frame, v_frame,                             & ! velocity frame correction
            u,v,                                          & ! 3D atm grid arrays in
            ph,phb,                                       &
            z0,zs,                                        & ! 2D atm grid arrays in
            uah,vah,                                      & ! 2D atm grid out
            fp%vx,fp%vy)                                    ! fire grid arrays out

          call apply_windrf(                        &
            ifms,ifme,jfms,jfme,                    &
            ifts,ifte,jfts,jfte,                    &
            nfuel_cat,fp%vx,fp%vy)

        else
          call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fz0,'driver_phys:fz0')        
          call interpolate_wind2fire_height(id,       & ! to identify debugging prints and files if needed
            ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
            ims,ime, kms,kme, jms,jme,                    &
            ips,ipe,jps,jpe,                              &
            its,ite,jts,jte,                              &
            ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
            ifms, ifme, jfms, jfme,                       &
            ifps, ifpe, jfps, jfpe,                       & ! fire patch bounds
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        & ! atm/fire grid ratio
            u_frame, v_frame,                             & ! velocity frame correction
            u,v,ph,phb,                                   & ! input atmospheric arrays
            fz0,fwh,                                      & ! input fire arrays
            fp%vx,fp%vy)                                          ! output fire arrays

          if(fire_use_windrf.eq.1)then
            call apply_windrf(                      &
            ifms,ifme,jfms,jfme,                    &
            ifts,ifte,jfts,jfte,                    &
            nfuel_cat,fp%vx,fp%vy)
          endif

        endif


      endif
    endif

!   the following executes in any case
    call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,lfn,'lfn')
    call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,tign,'tign')

    call sfire_model (id,ifun,restart,  &
        num_ignitions,                          & ! switches
        ifuelread,nfuel_cat0,                   & ! initialize fuel categories
        ifds,ifde,jfds,jfde,                    & ! fire domain dims
        ifms,ifme,jfms,jfme,                    & ! fire memory dims
        ifps,ifpe,jfps,jfpe,                    &
        ifts,ifte,jfts,jfte,                    & ! fire patch dims
        time_start,dt,                          & ! time and increment
        dxf,dyf,                                & ! fire mesh spacing
        ignition_line,                          & ! description of ignition lines
        ignitions_done_tile(ij),ignited_tile(1,ij),  &
        fxlong,fxlat,unit_fxlong,unit_fxlat,      & ! fire mesh coordinates
        lfn,lfn_out,tign,fuel_frac,                     & ! state: level function, ign time, fuel left
        fire_area,                              & ! output: fraction of cell burning
        fgrnhfx,fgrnqfx,                        & ! output: heat fluxes
        ros,ros0,rosx,rosy,                     & ! fire spread diagnostic variables
        fintens,fintens2,fdintens,fdintens2,    & ! fireline intensities
        nfuel_cat,                              & ! fuel data per point 
        fuel_time,fwh,fz0,                      & ! save derived internal data
        fp                                      & ! fire coefficients
    )

    
     if(ifun.eq.2)then
        call setup_wind_log_interpolation(           &
            ids,ide,  jds,jde,                    & ! atm grid dimensions
            ims,ime,  jms,jme,                    &
            ips,ipe,jps,jpe,                              &
            its,ite,jts,jte,                              &
            ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
            ifms, ifme, jfms, jfme,                       &
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        & ! atm/fire grid ratio
            z0,                                           & ! atm grid arrays in
            nfuel_cat,                              & ! fuel data per point 
            fz0,fwh)                                  ! fire arrays out

    
    elseif(ifun.eq.6)then ! heat fluxes into the atmosphere    
    
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnhfx,'fire_driver:fgrnhfx')
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnqfx,'fire_driver:fgrnqfx')
    
        ! sum the fluxes over atm cells
        if(use_atm_vars)then                                  
          call sum_2d_cells(        &
            ifms,ifme,jfms,jfme,  &
            ifts,ifte,jfts,jfte,  &
            fuel_frac,              &
            ims, ime, jms, jme,   &
            its,ite,jts,jte,      &
            avg_fuel_frac)
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
          s = 1./(ir*jr)
          do j=jts,jte
            do i=its,ite
                ! scale ground fluxes to get the averages
                avg_fuel_frac(i,j)=avg_fuel_frac(i,j)*s
                grnhfx(i,j)=fire_atm_feedback*grnhfx(i,j)*s
                grnqfx(i,j)=fire_atm_feedback*grnqfx(i,j)*s
                ! we do not have canopy fluxes yet...
                canhfx(i,j)=0
                canqfx(i,j)=0
            enddo
          enddo

          call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnhfx,'fire_driver:grnhfx')
          call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnqfx,'fire_driver:grnqfx')
       endif

    endif ! ifun=6

enddo ! tiles
!$OMP END PARALLEL DO

#ifdef DEBUG_OUT
if(ifun.eq.1)then
    if(pid.ne.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,zs,'zs',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%zsf,'zsf',pid)
    endif
endif
#endif

if (ifun.eq.4)then
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
        call wrf_dm_maxval(ignited_patch(i),idex,jdex)
        call message('sfire_driver_phys: collected')
#endif
        if(ignited_patch(i).eq.0)then
            call crash('sfire_driver_phys: Ignition failed, no nodes ignited. Bad coordinates?')
        endif
    enddo

 !call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,1,0,0,uah,'uah')
 !call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,1,vah,'vah')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fp%vx,'uf')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fp%vy,'vf')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,lfn,'lfn')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,tign,'tign')
#ifdef DEBUG_OUT
    if(pid.gt.0)then
 !       call write_array_m(ips,ipe1,jps,jpe,ims,ime,jms,jme,uah,'uah',pid)
 !       call write_array_m(ips,ipe,jps,jpe1,ims,ime,jms,jme,vah,'vah',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnhfx,'grnhfx',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnqfx,'grnqfx',pid)
        call write_array_m3(ips,ipe1,kds,kde+1,jps,jpe,ims,ime,kms,kme,jms,jme,u,'u',pid)
        call write_array_m3(ips,ipe,kds,kde+1,jps,jpe1,ims,ime,kms,kme,jms,jme,v,'v',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%vx,'uf',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%vy,'vf',pid)
    endif
#endif
endif

if(ifun.eq.5)then
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,lfn,'lfn',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,tign,'tign',pid)
    endif
#endif
endif

if(ifun.eq.6)then
    call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fgrnhfx,'fgrnhfx')
    call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fgrnqfx,'fgrnqfx')
    call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,0,grnhfx,'grnhfx')
    call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,0,grnqfx,'grnqfx')
#ifdef DEBUG_OUT
    if(pid.gt.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnhfx,'grnhfx',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnqfx,'grnqfx',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fuel_frac,'fuel_frac',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnhfx,'fgrnhfx',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnqfx,'fgrnqfx',pid)
    endif
#endif
endif

end subroutine sfire_driver_phys
!
!*******************
!

subroutine fire_ignition_convert (config_flags,fire_max_ignitions,fire_ignition_longlat, &
    ignition_line,fire_num_ignitions,unit_fxlong,unit_fxlat)
    USE module_configure
    implicit none
! create ignition arrays from scalar flags
!*** arguments
    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in)::fire_max_ignitions
    TYPE (ignition_line_type), DIMENSION(fire_max_ignitions), intent(out):: ignition_line ! any values from input discarded 
    integer, intent(out)::fire_num_ignitions,fire_ignition_longlat
    real, intent(out)::unit_fxlong,unit_fxlat
!*** local
    integer::i
    logical:: real,ideal
    real::lat_ctr,lon_ctr
!*** executable
    ! this is only until I figure out how to input arrays through the namelist...
    if(fire_max_ignitions.lt.5)call crash('fire_max_ignitions too small')
    ! figure out which kind of coordinates from the first given
    ideal=config_flags%fire_ignition_start_x1 .ne.0. .or. config_flags%fire_ignition_start_y1 .ne. 0.
    real=config_flags%fire_ignition_start_lon1 .ne. 0. .or. config_flags%fire_ignition_start_lat1 .ne. 0.
    if(ideal)call message('Using ideal ignition coordinates, m from the lower left domain corner')
    if(real)call message('Using real ignition coordinates, longitude and latitude')
    if(ideal.and.real)call crash('Only one of the ideal or real coordinates may be given')

    fire_ignition_longlat=0  ! default, if no ignition
    if(ideal)then
       ! use values from _x and _y variables
       fire_ignition_longlat=0
       ignition_line(1)%start_x=config_flags%fire_ignition_start_x1
       ignition_line(1)%start_y=config_flags%fire_ignition_start_y1
       ignition_line(1)%end_x=config_flags%fire_ignition_end_x1
       ignition_line(1)%end_y=config_flags%fire_ignition_end_y1
       ignition_line(2)%start_x=config_flags%fire_ignition_start_x2
       ignition_line(2)%start_y=config_flags%fire_ignition_start_y2
       ignition_line(2)%end_x=config_flags%fire_ignition_end_x2
       ignition_line(2)%end_y=config_flags%fire_ignition_end_y2
       ignition_line(3)%start_x=config_flags%fire_ignition_start_x3
       ignition_line(3)%start_y=config_flags%fire_ignition_start_y3
       ignition_line(3)%end_x=config_flags%fire_ignition_end_x3
       ignition_line(3)%end_y=config_flags%fire_ignition_end_y3
       ignition_line(4)%start_x=config_flags%fire_ignition_start_x4
       ignition_line(4)%start_y=config_flags%fire_ignition_start_y4
       ignition_line(4)%end_x=config_flags%fire_ignition_end_x4
       ignition_line(4)%end_y=config_flags%fire_ignition_end_y4
       ignition_line(5)%start_x=config_flags%fire_ignition_start_x5
       ignition_line(5)%start_y=config_flags%fire_ignition_start_y5
       ignition_line(5)%end_x=config_flags%fire_ignition_end_x5
       ignition_line(5)%end_y=config_flags%fire_ignition_end_y5
    endif
    if(real)then
        ! use values from _long and _lat
       fire_ignition_longlat=1
       ignition_line(1)%start_x=config_flags%fire_ignition_start_lon1
       ignition_line(1)%start_y=config_flags%fire_ignition_start_lat1
       ignition_line(1)%end_x=config_flags%fire_ignition_end_lon1
       ignition_line(1)%end_y=config_flags%fire_ignition_end_lat1
       ignition_line(2)%start_x=config_flags%fire_ignition_start_lon2
       ignition_line(2)%start_y=config_flags%fire_ignition_start_lat2
       ignition_line(2)%end_x=config_flags%fire_ignition_end_lon2
       ignition_line(2)%end_y=config_flags%fire_ignition_end_lat2
       ignition_line(3)%start_x=config_flags%fire_ignition_start_lon3
       ignition_line(3)%start_y=config_flags%fire_ignition_start_lat3
       ignition_line(3)%end_x=config_flags%fire_ignition_end_lon3
       ignition_line(3)%end_y=config_flags%fire_ignition_end_lat3
       ignition_line(4)%start_x=config_flags%fire_ignition_start_lon4
       ignition_line(4)%start_y=config_flags%fire_ignition_start_lat4
       ignition_line(4)%end_x=config_flags%fire_ignition_end_lon4
       ignition_line(4)%end_y=config_flags%fire_ignition_end_lat4
       ignition_line(5)%start_x=config_flags%fire_ignition_start_lon5
       ignition_line(5)%start_y=config_flags%fire_ignition_start_lat5
       ignition_line(5)%end_x=config_flags%fire_ignition_end_lon5
       ignition_line(5)%end_y=config_flags%fire_ignition_end_lat5
    endif
    ! common to both cases
       ignition_line(1)%ros=config_flags%fire_ignition_ros1 
       ignition_line(1)%radius=config_flags%fire_ignition_radius1 
       ignition_line(1)%start_time=config_flags%fire_ignition_start_time1 
       ignition_line(1)%end_time=config_flags%fire_ignition_end_time1 
       ignition_line(2)%ros=config_flags%fire_ignition_ros2 
       ignition_line(2)%radius=config_flags%fire_ignition_radius2 
       ignition_line(2)%start_time=config_flags%fire_ignition_start_time2 
       ignition_line(2)%end_time=config_flags%fire_ignition_end_time2 
       ignition_line(3)%ros=config_flags%fire_ignition_ros3 
       ignition_line(3)%radius=config_flags%fire_ignition_radius3 
       ignition_line(3)%start_time=config_flags%fire_ignition_start_time3 
       ignition_line(3)%end_time=config_flags%fire_ignition_end_time3 
       ignition_line(4)%ros=config_flags%fire_ignition_ros4 
       ignition_line(4)%radius=config_flags%fire_ignition_radius4 
       ignition_line(4)%start_time=config_flags%fire_ignition_start_time4 
       ignition_line(4)%end_time=config_flags%fire_ignition_end_time4 
       ignition_line(5)%ros=config_flags%fire_ignition_ros5 
       ignition_line(5)%radius=config_flags%fire_ignition_radius5 
       ignition_line(5)%start_time=config_flags%fire_ignition_start_time5
       ignition_line(5)%end_time=config_flags%fire_ignition_end_time5

    ! 
        fire_num_ignitions=0      
        do i=1,min(5,config_flags%fire_num_ignitions)
            ! count the ignitions 
            if(ignition_line(i)%radius.gt.0.)fire_num_ignitions=i
            ! expand ignition data given as zero
            if(ignition_line(i)%end_x.eq.0.)ignition_line(i)%end_x=ignition_line(i)%start_x
            if(ignition_line(i)%end_y.eq.0.)ignition_line(i)%end_y=ignition_line(i)%start_y
            if(ignition_line(i)%end_time.eq.0.)ignition_line(i)%end_time=ignition_line(i)%start_time
        enddo

    if(fire_ignition_longlat .eq. 0)then
       ! ideal
       !  ignition is in m
       unit_fxlong=1.  
       unit_fxlat=1.
       ! will set fire mesh coordinates to uniform mesh below
    else
       ! real
       lat_ctr=config_flags%cen_lat
       lon_ctr=config_flags%cen_lon
       ! 1 degree in m (approximate OK)
       unit_fxlat=pi2/(360.*reradius)  ! earth circumference in m / 360 degrees
       unit_fxlong=cos(lat_ctr*pi2/360.)*unit_fxlat  ! latitude
    endif

end subroutine fire_ignition_convert

!
!*****************************
!


!
!***
!

subroutine check_fmesh(ids,ide,ifds,ifde,ir,s)
!*** purpose: check if fire and atm meshes line up
implicit none
!*** arguments
integer, intent(in)::ids,ide,ifds,ifde,ir
character(len=*),intent(in)::s
!*** local
character(len=128)msg
!*** executable
if ((ide-ids+1)*ir.ne.(ifde-ifds+1))then
!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
    write(msg,1)s,ids,ide,ifds,ifde,ir
1   format('module_fr_sfire_driver: incompatible bounds ',a,' atm ',i5,':',i5,' fire ',i5,':',i5,' ratio ',i3)    
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
    call crash(msg)
endif
end subroutine check_fmesh

!
!*****************************
!
subroutine set_flags(config_flags)
USE module_configure
use module_fr_sfire_util
implicit none
TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
! copy flags from wrf to module_fr_sfire_util
! for instructions how to add a flag see the top of module_fr_sfire_util.F

fire_perimeter_time     = config_flags%fire_perimeter_time
fire_print_msg          = config_flags%fire_print_msg
fire_print_file         = config_flags%fire_print_file
fuel_left_method        = config_flags%fire_fuel_left_method
fuel_left_irl           = config_flags%fire_fuel_left_irl
fuel_left_jrl           = config_flags%fire_fuel_left_jrl
fire_const_time         = config_flags%fire_const_time
fire_const_grnhfx       = config_flags%fire_const_grnhfx
fire_const_grnqfx       = config_flags%fire_const_grnqfx
fire_atm_feedback       = config_flags%fire_atm_feedback
boundary_guard          = config_flags%fire_boundary_guard
fire_back_weight        = config_flags%fire_back_weight
fire_grows_only         = config_flags%fire_grows_only
fire_upwinding          = config_flags%fire_upwinding
fire_viscosity          = config_flags%fire_viscosity 
fire_lfn_ext_up         = config_flags%fire_lfn_ext_up 
fire_test_steps         = config_flags%fire_test_steps 
!fire_topo_from_atm     = config_flags%fire_topo_from_atm
fire_advection          = config_flags%fire_advection
fire_wind_log_interp    = config_flags%fire_wind_log_interp
fire_use_windrf         = config_flags%fire_use_windrf




end subroutine set_flags

end module module_fr_sfire_driver
