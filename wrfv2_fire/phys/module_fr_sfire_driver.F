!WRF:MEDIATION_LAYER:FIRE_MODEL

!*** Jan Mandel August 2007 - February 2008
!*** email: jmandel@ucar.edu or Jan.Mandel@gmail.com or Jan.Mandel@cudenver.edu

! This module is the only entry point from WRF-ARW to the wildland 
! fire model. The call to sfire_driver advances the fire model by 
! one timestep. The fire model inputs the wind and outputs 
! temperature and humidity tendencies. The fire model also inputs a 
! number of constant arrays (fuel data, topography). Additional 
! arguments are model state (for data assimilation) and constant arrays 
! the model gives to WRF for safekeeping because it is not allowed 
! to save anything.

! This model is described in [1]. The fire model is coupled with WRF 
! but the fire code itself is not dependent on WRF in any way other 
! than calls to few WRF utilities from module_fr_sfire_util. This 
! model uses a level set function method for advancing the fireline. 
! It is a reimplementation of an earlier model, which used fireline 
! propagation by tracers and was coupled with the Clark-Hall 
! atmospheric code, described in [2]. For WRF documentation see [3].

! Acknowledgements: Contributions to the level set method by Mijeong 
! Kim. The fire physics is adapted from an earlier code by Terry  
! L. Clark, Janice L. Coen, and Don Latham. The coupling with WRF is 
! adapted from a code by Ned Patton for coupling of the earlier fire
! model with WRF, with contributions by Jonathan D. Beezley. The
! WRF build and execution environment was set up by Jonathan Beezley.

! [1] Jan Mandel, Jonathan D. Beezley, Janice L. Coen, and Minjeong Kim,
! Data Asimilation for Wildland Fires: Ensemble Kalman filters in 
! coupled atmosphere-surface models, IEEE Control Systems Magazine, 
! submitted, 2007

! [2] T. L. Clark, J. Coen, and D. Latham, Description of a coupled 
! atmosphere-fire model, Intl. J. Wildland Fire, vol. 13, pp. 49–64, 
! 2004
!
! [3] http://www.mmm.ucar.edu/wrf/OnLineTutorial/Introduction/index.html

module module_fr_sfire_driver

use module_model_constants, only: cp,xlv
use module_fr_sfire_model
use module_fr_sfire_phys
use module_fr_sfire_atm
use module_fr_sfire_util

contains


subroutine sfire_driver_em ( grid , config_flags                   & 
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                        &
            ,ifms,ifme, jfms,jfme                        &
            ,ifps,ifpe, jfps,jfpe                       &
            ,rho,z_at_w,dz8w                            &
)
!*** purpose: driver from grid structure

! Driver layer modules
    USE module_domain
    USE module_configure
    USE module_driver_constants
    USE module_machine
    USE module_tiles
    USE module_dm

    implicit none
!*** arguments
    TYPE(domain) , TARGET          :: grid   ! data
    !  Structure that contains run-time configuration (namelist) data for domain
    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                        &
            ,ifms,ifme, jfms,jfme                        &
            ,ifps,ifpe, jfps,jfpe
    real,intent(in),dimension(ims:ime, kms:kme, jms:jme)::rho,  &! air density  (kg/m^3) (cell based, atm grid) 
                                z_at_w,dz8w                 ! ????????

!*** local
    INTEGER:: fire_num_ignitions,fire_ignition_longlat
    integer, parameter::fire_max_ignitions=5
    REAL, DIMENSION(fire_max_ignitions)::  fire_ignition_start_x, &
        fire_ignition_start_y, &
        fire_ignition_end_x, &
        fire_ignition_end_y, &
        fire_ignition_time, &
        fire_ignition_radius
    integer::fire_ifun,fire_ifun_start,ir,jr
    logical::need_lfn_update
    !real, dimension(ifms:ifme, jfms:jfme)::uf,vf,lfn_out  
    ! uf vf only do not need to be in the state but we need halo on them
    real, dimension(ifms:ifme, jfms:jfme)::lfn_out  
    integer, dimension(ifms:ifme, jfms:jfme)::nfuel_cat_int

!*** executable
    call fire_ignition_convert (config_flags,fire_max_ignitions,fire_ignition_longlat, &
        fire_ignition_start_x,fire_ignition_start_y,fire_ignition_end_x,fire_ignition_end_y, &
        fire_ignition_radius,fire_ignition_time,fire_num_ignitions)

    ir=grid%sr_x ! refinement ratio
    jr=grid%sr_y
    
    fire_ifun_start=1  ! initialize?
    if(grid%itimestep.ne.1)fire_ifun_start=3  ! should find a better distinction when to initialize

    need_lfn_update=.false.
    do fire_ifun=fire_ifun_start,6

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
#include HALO_FIRE_LONGLAT.inc
        elseif(fire_ifun.eq.2)then
!           halo exchange on zsf width 2
#include "HALO_FIRE_ZSF.inc"
        elseif(fire_ifun.eq.3)then
            if(fire_ifun_start<3)then
!               halo exchange on all fuel data width 1
#include "HALO_FIRE_FUEL.inc"
            endif
!           halo exchange on atm winds width 1
#include "HALO_FIRE_WIND_A.inc"
        elseif(fire_ifun.eq.4)then
!           halo exchange on fire winds width 1
#include "HALO_FIRE_WIND_F.inc"
        endif
#endif
        nfuel_cat_int(:,:)=int(grid%nfuel_cat)
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
            grid%itimestep,config_flags%fire_fuel_read,config_flags%fire_fuel_cat, &  ! in scalars
            grid%dt,grid%dx,grid%dy,                    &
            grid%u_frame,grid%v_frame,                  &
            config_flags%fire_fuel_left_method,         &
            config_flags%fire_fuel_left_irl,         &
            config_flags%fire_fuel_left_jrl,         &
            config_flags%fire_ext_grnd,config_flags%fire_ext_crwn,config_flags%fire_crwn_hgt, &
            config_flags%fire_boundary_guard,         &
            fire_num_ignitions,                                & 
            fire_ignition_start_x,fire_ignition_start_y, & ! ignition - small arrays
            fire_ignition_end_x,fire_ignition_end_y,     &
            fire_ignition_radius,fire_ignition_time,     &
            fire_ignition_longlat,                       &
            grid%em_u_2,grid%em_v_2,grid%em_mut,rho,grid%ht,      & ! in arrays, on atm grid
            z_at_w,dz8w,                                  &
            grid%xlong,grid%xlat,                         & ! coordinates of atm grid centers, for ignition location           
            grid%lfn,grid%tign_g,grid%fuel_frac,          & ! state arrays, fire grid
            grid%uf,grid%vf,lfn_out,                      & ! arrays to persist only over one timestep    
            grid%rthfrten,grid%rqvfrten,                & ! out arrays, atm grid
            grid%grnhfx,grid%grnqfx,grid%canhfx,grid%canqfx, & ! out redundant arrays, atm grid
            grid%fgrnhfx,grid%fgrnqfx,grid%fcanhfx,grid%fcanqfx, & ! out redundant arrays, atm grid
            grid%fxlong,grid%fxlat,                           &       
            nfuel_cat_int,                               & ! input, or internal for safekeeping
            grid%fuel_time,grid%zsf,                      & 
            grid%dzfsdx,grid%dzfsdy,                      &
            grid%bbb,grid%betafl,grid%phiwc,grid%r_0,grid%fgip,grid%ischap&
        )
        grid%nfuel_cat(:,:)=real(nfuel_cat_int(:,:))
    
    enddo

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
    itimestep,ifuelread,nfuel_cat0,dt,dx,dy,      & ! in scalars
    u_frame,v_frame,                              &
    fire_fuel_left_method,         &
    fire_fuel_left_irl,         &
    fire_fuel_left_jrl,         &
    fire_ext_grnd,fire_ext_crwn,fire_crwn_hgt,                              &
    boundary_guard,                               & 
    num_ignitions,                                & 
    ignition_start_x,ignition_start_y,            & ! ignition - small arrays
    ignition_end_x,ignition_end_y,                &
    ignition_radius,                              &
    ignition_time,                                &
    ignition_longlat,                             &
    u,v,mu,rho,zs,                                & ! in arrays, atm grid
    z_at_w,dz8w,                                  &
    xlong,xlat,                                   &
    lfn,tign,fuel_frac,                           & ! state arrays, fire grid
    uf,vf,lfn_out,                                & ! fire wind velocities, out level set function    
    rthfrten,rqvfrten,                            & ! out arrays, atm grid
    grnhfx,grnqfx,canhfx,canqfx,                  & ! out redundant arrays, atm grid  
    fgrnhfx,fgrnqfx,fcanhfx,fcanqfx,              & ! out redundant arrays, fire grid
    fxlong,fxlat,                            & !  
    unit_fxlong, unit_fxlat,                             &                            
    nfuel_cat,                                    & ! in array, data, fire grid, or constant internal
    fuel_time,zsf,                                & ! save constant internal data, fire grid
    dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip,ischap&
    )

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
    fire_fuel_left_method,                        & ! for fuel_left
    fire_fuel_left_irl,                           &
    fire_fuel_left_jrl,                           &
    num_tiles                                       ! number of tiles
    
logical, intent(out)::need_lfn_update

integer,dimension(num_tiles),intent(in) :: i_start,i_end,j_start,j_end  ! atm grid tiling

real, intent(in):: &
    dt,                                           & ! time step
    dx,dy,                                        & ! atm grid step
    u_frame,v_frame,                              & ! velocity offset
    fire_crwn_hgt,                                & ! lowest height crown fire heat is released (m)
    fire_ext_grnd,                                & ! extinction depth of ground fire heat (m)
    fire_ext_crwn                                   !  extinction depth of crown fire heat (m)

integer, intent(in):: boundary_guard                ! stop when fire gets this far from the boundary

integer, intent(in):: num_ignitions                 ! number of ignitions, can be 0
real, dimension(num_ignitions), intent(in):: &   
    ignition_start_x,ignition_start_y, &
    ignition_end_x,ignition_end_y,ignition_radius, & ! start, end, radius, time
    ignition_time                           !  of ignition lines
integer, intent(in):: ignition_longlat       ! if 1 ignition give as long/lat, otherwise as m from lower left corner

real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::u,v ! wind velocity (m/s) (node based, atm grid) 
real,intent(in),dimension(ims:ime,jms:jme)::mu          ! dry air mass (Pa)  pressure??  (cell based, atm grid)
real,intent(in),dimension(ims:ime, jms:jme)::  zs ! terrain height  
real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::rho, &  ! air density  (kg/m^3) (cell based, atm grid) 
                z_at_w,dz8w                         ! height of some sort??

real, dimension(ims:ime, jms:jme), intent(in)::xlong, xlat ! 
    
integer, intent(inout), dimension(ifms:ifme,jfms:jfme):: &
    nfuel_cat                                       ! fuel data; can be also set inside (cell based, fire grid)

real, intent(inout), dimension(ifms:ifme, jfms:jfme)::     &
    lfn,tign,fuel_frac,                        &     ! state: level function, ign time, fuel left
    uf,vf,lfn_out                                    ! fire wind velocities

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
    fcanqfx                                         ! moisture flux from crown fire (W/m^2) 

!  ***** data (constant in time) *****

real, dimension(ifms:ifme, jfms:jfme), intent(inout)::fxlong,fxlat ! fire mesh coordinates
real, intent(in):: unit_fxlong,unit_fxlat                  ! their unit in m  

real, intent(inout), dimension(ifms:ifme, jfms:jfme):: &
    fuel_time,zsf,                               &
    dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip
integer, intent(inout), dimension(ifms:ifme, jfms:jfme):: ischap
    
!*** local
real :: dxf,dyf,time_start,latm
integer :: its,ite,jts,jte,kts,kte, &            ! tile
    ij,i,j,k,id,pid,kpe1, &
    ifts,ifte,jfts,jfte                          ! fire tile
character(len=128)::msg
character(len=3)::kk


!*** executable

! time - assume dt does not change
time_start = itimestep * dt

! fire mesh step
dxf=dx/ir
dyf=dy/jr


! set up fire mesh coordinates for ideal case

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

! check mesh dimensions
call check_fmesh(ids,ide,ifds,ifde,ir,'id')           ! check if atm and fire grids line up
call check_fmesh(jds,jde,jfds,jfde,jr,'jd')
call check_fmesh(ips,ipe,ifps,ifpe,ir,'ip')
call check_fmesh(jps,jpe,jfps,jfpe,jr,'jp')
call check_mesh_2dim(ips,ipe,jps,jpe,ims,ime,jms,jme)        ! check if atm patch fits in atm array
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme) ! check if fire patch fits in fire array
call check_mesh_2dim(ips,ipe,jps,jpe,ids,ide,jds,jde)        ! check if atm patch fits in atm domain
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifds,ifde,jfds,jfde) ! check if fire patch fits in fire domain

! init rest of fuel tables with derived constants
if(ifun.eq.1)call init_fuel_cats  ! common for all threads

pid=0
if(itimestep.le.10.or.mod(itimestep,10).eq.0)pid=itimestep

if(ignition_longlat .eq.0)then
   !  ignition is in m
   unit_fxlong=1.  
   unit_fxlat=1.
   ! will set fire mesh coordinates to uniform mesh below
else
   ! check for zero long 
   if(xlong(ips,jps).eq.0. .and. xlong(ipe,jpe).eq. 0.)call crash('sfire_driver_phys: long lat not set, run real not ideal')
   ! latitude in the middle of the patch
   latm=xlat((ips+ipe)/2,(jps+jpe)/2)
   ! 1 degree in m (approximate OK)
   unit_fxlat=6378*2*3.14159/360.   ! earth circumference in m / 360 degrees
   unit_fxlong=cos(latm*3.14159/180.)*unit_fxlat  ! latitude
   ! will interpolate nodal coordinates to the fire mesh 
endif


! set up fire tiles & interpolate to fire grid
!OMP PARALLEL DO PRIVATE(ij,its,ite,jts,jte,ifts,ifte,jfts,jfte,msg,id)
do ij=1,num_tiles

    id=0  ! do not print/write anything
    if(itimestep.le.10.or.mod(itimestep,10).eq.0)id=itimestep+ij*10000


    ! set up tile bounds    
    its = i_start(ij)  ! start atmospheric tile in i
    ite = i_end(ij)    ! end atmospheric tile in i
    jts = j_start(ij)  ! start atmospheric tile in j
    jte = j_end(ij)    ! end atmospheric tile in j
    ifts= (its-ids)*ir+ifds       ! start fire tile in i
    ifte= (ite-ids+1)*ir+ifds-1   ! end fire tile in i
    jfts= (jts-jds)*jr+jfds       ! start fire tile in j
    jfte= (jte-jds+1)*jr+jfds-1   ! end fire tile in j
        
    write(msg,*)'tile=',ij,' id=',id,' ifun=',ifun
    call message(msg)
    write(msg,7001)'atm tile   ','its',its,ite,jts,jte
    call message(msg)                   
    write(msg,7001)'fire tile  ','ifts',ifts,ifte,jfts,jfte
    call message(msg)                    

    ! check the tiles
    call check_mesh_2dim(its,ite,jts,jte,ips,ipe,jps,jpe)                 ! check if atm tile fits in atm patch
    call check_mesh_2dim(ifts,ifte,jfts,jfte,ifps,ifpe,jfps,jfpe)         ! check if fire tile fits in fire patch
    call check_mesh_2dim(ifts-2,ifte+2,jfts-2,jfte+2,ifms,ifme,jfms,jfme)! check if fire node tile fits in memory

! atm bounds 
! fake tile bounds
kts=kps
kte=kpe

write(msg,'(a,i6,a,2(f15.6,a))')'time step',itimestep,' at',time_start,' duration',dt,'s'
call message(msg)
7001 format(a,' dimensions ',a4,':',i6,' to ',i6,' by ',i6,' to ',i6)
write(msg,'(a,2i9)')'refinement ratio:',ir,jr
    if(ifun.eq.1)then   ! set terrain

        call print_2d_stats(ips,ipe,jps,jpe,ims,ime,jms,jme,zs,'driver:zs')
    
        ! interpolate terrain height
        
        call interpolate_z2fire(id,                 & ! for debug output, <= 0 no output
            ids,ide,  jds,jde,                    & ! atm grid dimensions
            ims,ime,  jms,jme,                    &
            its,ite,jts,jte,                              &
            ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
            ifms, ifme, jfms, jfme,                       &
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        & ! atm/fire grid ratio
            zs,                                       & ! atm grid arrays in
            zsf)                                      ! fire grid arrays out

        if(ignition_longlat .eq.0)then
            ! set ideal fire mesh coordinates

    	    if(ignition_longlat .eq.0)then
                !  ignition is in m
                do j=jfts,jfte
                    do i=ifts,ifte
                        ! uniform mesh, lower left domain corner is (0,0)
                        fxlong(i,j)=(i-ifds+0.5)*dxf 
                        fxlat (i,j)=(j-jfds+0.5)*dyf
                    enddo
                enddo
            endif
        else
            ! assume halo xlong xlat
            ! interpolate nodal coordinates

            ! first extend the coordinates by one beyond the domain boundary 
            call continue_at_boundary(1,1, & ! do x direction or y direction
                ims,ime,  jms,jme,           &                ! memory dims
                ids,ide,jds,jde, &            ! domain dims - winds defined up to +1
                its,ite,jts,jte, &                ! tile dims
                xlat)                               ! array

            call continue_at_boundary(1,1, & ! do x direction or y direction
                ims,ime,  jms,jme,           &                ! memory dims
                ids,ide,jds,jde, &            ! domain dims - winds defined up to +1
                its,ite,jts,jte, &                ! tile dims
                xlong)                               ! array

            call interpolate_2d(  &
                ims,ime,  jms,jme,                    & ! memory dimension on atm grid
                its-1,ite+1,jts-1,jte+1,                      & ! where atm grid values known - need halo and extension!
                ifms,ifme,jfms,jfme,&  ! array dims fire grid
                ifts,ifte,jfts,jfte,&  ! dimensions on the fire grid to interpolate to
                ir,jr,                  & ! refinement ratio
                real(ids),real(jds),ifds+(ir+1)*.5,jfds+(jr+1)*.5, & ! line up by lower left corner of domain
                xlat,                     & ! in atm grid     
                fxlat)                      ! out fire grid

            call interpolate_2d(  &
                ims,ime,  jms,jme,                    & ! memory dimension on atm grid
                its-1,ite+1,jts-1,jte+1,                      & ! where atm grid values known - need halo and extension!
                ifms,ifme,jfms,jfme,&  ! array dims fire grid
                ifts,ifte,jfts,jfte,&  ! dimensions on the fire grid to interpolate to
                ir,jr,                  & ! refinement ratio
                real(ids),real(jds),ifds+(ir+1)*.5,jfds+(jr+1)*.5, & ! line up by lower left corner of domain
                xlong,                     & ! in atm grid     
               fxlong)                      ! out fire grid
         endif

               
    elseif(ifun.eq.3)then  ! interpolate winds to the fire grid
    
        call interpolate_atm2fire(id,                     & ! flag for debug output
            ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
            ims,ime, kms,kme, jms,jme,                    &
            its,ite,jts,jte,                              &                    
            ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
            ifms, ifme, jfms, jfme,                       &
            ifts,ifte,jfts,jfte,                          &
            ir,jr,                                        & ! atm/fire grid ratio
            u_frame, v_frame,                             & ! velocity frame correction
            u,v,                                       & ! atm grid arrays in
            uf,vf)                                      ! fire grid arrays out
    
    endif

    call sfire_model (id,ifun,need_lfn_update,  &
        num_ignitions,                          & ! switches
        ifuelread,nfuel_cat0,                   & ! initialize fuel categories
        fire_fuel_left_method,                  &
        fire_fuel_left_irl,                     &
        fire_fuel_left_jrl,                     &
        ifds,ifde,jfds,jfde,                    & ! fire domain dims
        ifms,ifme,jfms,jfme,                    & ! fire memory dims
        ifts,ifte,jfts,jfte,                    & ! fire patch dims
        time_start,dt,                          & ! time and increment
        dxf,dyf,                                & ! fire mesh spacing
        boundary_guard,                         &
        ignition_start_x,ignition_start_y,      & ! ignition - small arrays
        ignition_end_x,ignition_end_y,          &
        ignition_radius,                        &
        ignition_time,                          &
        coord_xf,coord_yf,unit_fxlong,unit_fxlat,      & ! fire mesh coordinates
        zsf,                                    & ! terrain height (for gradient)
        uf,vf,                                  & ! input: wind
        lfn,lfn_out,tign,fuel_frac,                     & ! state: level function, ign time, fuel left
        fgrnhfx,fgrnqfx,                        & ! output: heat fluxes
        nfuel_cat,                              & ! fuel data per point 
        fuel_time,                              & ! save derived internal data
        dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip,ischap &
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

        do j=jts,jte
            do i=its,ite
                ! scale ground fluxes to get the averages
                grnhfx(i,j)=grnhfx(i,j)/(ir*jr)
                grnqfx(i,j)=grnqfx(i,j)/(ir*jr)
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

if(ifun.eq.1)then
    call print_2d_stats(ifts-1,ifte+1,jfts-1,jfte+1,ifms,ifme,jfms,jfme,zsf,'driver_phys:zsf')        
    if(pid.ne.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,zs,'zs',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,zsf,'zsf',pid)
    endif
elseif(ifun.eq.3)then
    if(pid.gt.0)then
        call write_array_m3(ips,ipe+1,kds,kds+1,jps,jpe+1,ims,ime,kms,kme,jms,jme,u,'u',pid)
        call write_array_m3(ips,ipe+1,kds,kds+1,jps,jpe+1,ims,ime,kms,kme,jms,jme,v,'v',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,uf,'uf',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,vf,'vf',pid)
    endif
elseif(ifun.eq.5)then
    if(pid.gt.0)then
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,lfn,'lfn',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,tign,'tign',pid)
    endif
elseif(ifun.eq.6)then
    if(pid.gt.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnhfx,'grnhfx',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnqfx,'grnqfx',pid)
        !call write_array_m3(ips,ipe,kps,kpe,jps,jpe,ims,ime,kms,kme,jms,jme,rthfrten,'rthfrten',pid)
        !call write_array_m3(ips,ipe,kps,kpe,jps,jpe,ims,ime,kms,kme,jms,jme,rqvfrten,'rqvfrten',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fuel_frac,'fuel_frac',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnhfx,'fgrnhfx',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnqfx,'fgrnqfx',pid)
    endif
    !call print_2d_stats(ips,ipe,jps,jpe,ims,ime,jms,jme,mu,'driver:mu')
    !call print_3d_stats(ips,ipe,kps,kpe,jps,jpe,ims,ime,kms,kme,jms,jme,rho,'driver:rho')
    kpe1=min(kps+1,kpe)
    !kpe1=kps-1
    do k=kts,min(kte,kts+3)
        write(kk,'(i2)')k
        call print_3d_stats(ips,ipe,k,k,jps,jpe,ims,ime,kms,kme,jms,jme,rthfrten,kk//'driver_phys:rthfrten')
        call print_3d_stats(ips,ipe,k,k,jps,jpe,ims,ime,kms,kme,jms,jme,rqvfrten,kk//'driver_phys:rqvfrten')
    enddo
endif

end subroutine sfire_driver_phys
!
!*******************
!

subroutine fire_ignition_convert (config_flags,fire_max_ignitions,fire_ignition_longlat, &
    fire_ignition_start_x,fire_ignition_start_y,fire_ignition_end_x,fire_ignition_end_y, &
    fire_ignition_radius,fire_ignition_time,fire_num_ignitions)
    USE module_configure
    implicit none
!*** arguments
    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in)::fire_max_ignitions,fire_ignition_longlat
    real, dimension(fire_max_ignitions), intent(out):: &
        fire_ignition_start_x,fire_ignition_start_y,fire_ignition_end_x,fire_ignition_end_y, &
        fire_ignition_radius,fire_ignition_time
    integer, intent(out)::fire_num_ignitions
!*** local
    integer::i
!*** executable
    ! this is only until I figure out how to input arrays through the namelist...
    if(fire_max_ignitions.lt.5)call crash('fire_max_ignitions too small')
    if(fire_ignition_longlat .eq. 0)then
        % use values from _x and _y variables
        fire_ignition_start_x(1)=config_flags%fire_ignition_start_x1
        fire_ignition_start_y(1)=config_flags%fire_ignition_start_y1
        fire_ignition_end_x(1)=config_flags%fire_ignition_end_x1
        fire_ignition_end_y(1)=config_flags%fire_ignition_end_y1
        fire_ignition_start_x(2)=config_flags%fire_ignition_start_x2
        fire_ignition_start_y(2)=config_flags%fire_ignition_start_y2
        fire_ignition_end_x(2)=config_flags%fire_ignition_end_x2
        fire_ignition_end_y(2)=config_flags%fire_ignition_end_y2
        fire_ignition_start_x(3)=config_flags%fire_ignition_start_x3
        fire_ignition_start_y(3)=config_flags%fire_ignition_start_y3
        fire_ignition_end_x(3)=config_flags%fire_ignition_end_x3
        fire_ignition_end_y(3)=config_flags%fire_ignition_end_y3
        fire_ignition_start_x(4)=config_flags%fire_ignition_start_x4
        fire_ignition_start_y(4)=config_flags%fire_ignition_start_y4
        fire_ignition_end_x(4)=config_flags%fire_ignition_end_x4
        fire_ignition_end_y(4)=config_flags%fire_ignition_end_y4
        fire_ignition_start_x(5)=config_flags%fire_ignition_start_x5
        fire_ignition_start_y(5)=config_flags%fire_ignition_start_y5
        fire_ignition_end_x(5)=config_flags%fire_ignition_end_x3
        fire_ignition_end_y(5)=config_flags%fire_ignition_end_y3
    else
        % use values from _long and _lat
        fire_ignition_start_x(1)=config_flags%fire_ignition_start_long1
        fire_ignition_start_y(1)=config_flags%fire_ignition_start_lat1
        fire_ignition_end_x(1)=config_flags%fire_ignition_end_long1
        fire_ignition_end_y(1)=config_flags%fire_ignition_end_lat1
        fire_ignition_start_x(2)=config_flags%fire_ignition_start_long2
        fire_ignition_start_y(2)=config_flags%fire_ignition_start_lat2
        fire_ignition_end_x(2)=config_flags%fire_ignition_end_long2
        fire_ignition_end_y(2)=config_flags%fire_ignition_end_lat2
        fire_ignition_start_x(3)=config_flags%fire_ignition_start_long3
        fire_ignition_start_y(3)=config_flags%fire_ignition_start_lat3
        fire_ignition_end_x(3)=config_flags%fire_ignition_end_long3
        fire_ignition_end_y(3)=config_flags%fire_ignition_end_lat3
        fire_ignition_start_x(4)=config_flags%fire_ignition_start_long4
        fire_ignition_start_y(4)=config_flags%fire_ignition_start_lat4
        fire_ignition_end_x(4)=config_flags%fire_ignition_end_long4
        fire_ignition_end_y(4)=config_flags%fire_ignition_end_lat4
        fire_ignition_start_x(5)=config_flags%fire_ignition_start_long5
        fire_ignition_start_y(5)=config_flags%fire_ignition_start_lat5
        fire_ignition_end_x(5)=config_flags%fire_ignition_end_long3
        fire_ignition_end_y(5)=config_flags%fire_ignition_end_lat3
    endif
    % common to both cases
        fire_ignition_radius(1)=config_flags%fire_ignition_radius1 
        fire_ignition_time(1)=config_flags%fire_ignition_time1 
        fire_ignition_radius(2)=config_flags%fire_ignition_radius2 
        fire_ignition_time(2)=config_flags%fire_ignition_time2 
        fire_ignition_radius(3)=config_flags%fire_ignition_radius3 
        fire_ignition_time(3)=config_flags%fire_ignition_time3 
        fire_ignition_radius(4)=config_flags%fire_ignition_radius4 
        fire_ignition_time(4)=config_flags%fire_ignition_time4 
        fire_ignition_radius(5)=config_flags%fire_ignition_radius5 
        fire_ignition_time(5)=config_flags%fire_ignition_time5

    % 
        fire_num_ignitions=0      
        do i=1,min(5,config_flags%fire_num_ignitions)
            if(fire_ignition_radius(i).gt.0.)fire_num_ignitions=i
        enddo

end subroutine fire_ignition_convert

!
!*****************************
!
!module_fr_sfire_driver%%interpolate_atm2fire

subroutine interpolate_atm2fire(id,               & ! for debug output, <= 0 no output
    ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
    ims,ime, kms,kme, jms,jme,                    &
    its,ite,jts,jte,                              &
    ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
    ifms, ifme, jfms, jfme,                       &
    ifts,ifte,jfts,jfte,                          &
    ir,jr,                                        & ! atm/fire grid ratio
    u_frame, v_frame,                             & ! velocity frame correction
    u,v,                                          & ! atm grid arrays in
    uf,vf)                                          ! fire grid arrays out
    
implicit none
!*** purpose: interpolate winds and height

!*** arguments
integer, intent(in)::id,                          &
    ids,ide, kds,kde, jds,jde,                    & ! atm domain bounds
    ims,ime, kms,kme, jms,jme,                    & ! atm memory bounds 
    its,ite,jts,jte,                              & ! atm tile bounds
    ifds, ifde, jfds, jfde,                       & ! fire domain bounds
    ifms, ifme, jfms, jfme,                       & ! fire memory bounds
    ifts,ifte,jfts,jfte,                          & ! fire tile bounds
    ir,jr                                         ! atm/fire grid refinement ratio
real, intent(in):: u_frame, v_frame                 ! velocity frame correction
real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::&
    u,v                                             ! atm wind velocity, staggered  
real,intent(out), dimension(ifms:ifme,jfms:jfme)::&
    uf,vf                                           ! wind velocity fire grid nodes 
    
    
!*** local
#define TDIMS its-1,ite+2,jts-1,jte+2
real, dimension(its-1:ite+2,jts-1:jte+2):: ua,va   ! atm winds, averaged over height
integer:: i,j,k,ifts1,ifte1,jfts1,jfte1

!*** executable

    k=kds             ! the ground
    do j = jts-1,jte+2
        do i = its-1,ite+2 
            ! average 1st 2 layers, correct const shift
            ua(i,j)=0.5*( u(i,k,j) + u(i,k+1,j)) + u_frame
            va(i,j)=0.5*( v(i,k,j) + v(i,k+1,j)) + v_frame
        enddo
    enddo

    ! extend the winds by one beyond the domain boundary 
    call continue_at_boundary(1,0, & ! do x direction or y direction
    TDIMS,           &                ! memory dims
    ids,ide+1,jds,jde+1, &            ! domain dims - winds defined up to +1
    its,ite+1,jts,jte+1, &                ! tile dims
    va)                               ! array

    call continue_at_boundary(0,1, & ! do x direction or y direction
    TDIMS,           &                ! memory dims
    ids,ide+1,jds,jde+1, &            ! domain dims - winds defined up to +1
    its,ite+1,jts,jte+1, &                ! tile dims
    ua)                               ! array

!if (id.gt.0) then
!    call write_array_m(TDIMS,TDIMS,ua,'ua',id)
!    call write_array_m(TDIMS,TDIMS,va,'va',id)
!endif

call print_2d_stats_vec(its,ite+1,jts,jte+1,TDIMS,ua,va, &
    'driver: atm wind (m/s)')
    

!      ---------------
!     | F | F | F | F |   Example of atmospheric and fire grid with
!     |-------|-------|   ir=jr=4.
!     | F | F | F | F |   Winds are given at the midpoints of the sides of the coarse grid,
!     u-------z-------|   interpolated to midpoints of the cells of the fine fire grid F.
!     | F | F | F | F |
!     |---------------|
!     | * | F | F | F |
!      -------v-------
!
! Meshes are aligned by the lower left cell of the domain. Then in the above figure
! u = node with the ua component of the wind at (ids,jds), midpoint of side
! v = node with the va component of the wind at (ids,jds), midpoint of side
! * = fire grid node at (ifds,jfds)
! z = node with height, midpoint of cell
! 
! ua(ids,jds)=uf(ifds-0.5,jfds+jr/2+0.5)
! va(ids,jds)=vf(ifds+ir/2+0.5,jfds-0.5)
! za(ids,jds)=zsf(ifds+ir/2+0.5,jfds+jr/2+0.5)
    
    ifts1=snode(ifts,ifds,-1) ! go 1 beyond domain boundary but not between tiles
    ifte1=snode(ifte,ifde,+1)
    jfts1=snode(jfts,jfds,-1)
    jfte1=snode(jfte,jfde,+1)
    
    call interpolate_2d(  &
        TDIMS,                  & ! memory dims atm grid tile
        TDIMS,                  & ! where atm grid values set
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ifts1,ifte1,jfts1,jfte1,& ! dimensions on the fire grid to interpolate to
        ir,jr,                  & ! refinement ratio
        real(ids),real(jds),ifds-.5,jfds+(jr+1)*.5, & ! line up by lower left corner of domain
        ua,                     & ! in atm grid     
        uf)                      ! out fire grid

    call interpolate_2d(  &
        TDIMS,                  & ! memory dims atm grid tile
        TDIMS,                  & ! where atm grid values set
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ifts1,ifte1,jfts1,jfte1,& ! dimensions on the fire grid to interpolate to
        ir,jr,                  & ! refinement ratio
        real(ids),real(jds),ifds+(ir+1)*.5,jfds-0.5, & ! line up by lower left corner of domain
        va,                     & ! in atm grid     
        vf)                      ! out fire grid

!call print_2d_stats_vec(ifts-1,ifte+1,jfts-1,jfte+1,ifms,ifme,jfms,jfme,uf,vf,'fire wind (m/s)')


end subroutine interpolate_atm2fire

!
!*****************************
!

subroutine interpolate_z2fire(id,                 & ! for debug output, <= 0 no output
    ids,ide, jds,jde,                    & ! atm grid dimensions
    ims,ime, jms,jme,                    &
    its,ite,jts,jte,                              &
    ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
    ifms, ifme, jfms, jfme,                       &
    ifts,ifte,jfts,jfte,                          &
    ir,jr,                                        & ! atm/fire grid ratio
    zs,                                       & ! atm grid arrays in
    zsf)                                      ! fire grid arrays out
    
implicit none
!*** purpose: interpolate height

!*** arguments
integer, intent(in)::id,                          &
    ids,ide, jds,jde,                    & ! atm domain bounds
    ims,ime,jms,jme,                    & ! atm memory bounds 
    its,ite,jts,jte,                              & ! atm tile bounds
    ifds, ifde, jfds, jfde,                       & ! fire domain bounds
    ifms, ifme, jfms, jfme,                       & ! fire memory bounds
    ifts,ifte,jfts,jfte,                          & ! fire tile bounds
    ir,jr                                         ! atm/fire grid refinement ratio
real, intent(in), dimension(ims:ime, jms:jme):: zs  ! terrain height at atm cell centers                                        & ! terrain height  
real,intent(out), dimension(ifms:ifme,jfms:jfme)::&
    zsf                                             ! terrain height fire grid nodes
    
    
!*** local
real, dimension(its-2:ite+2,jts-2:jte+2):: za      ! terrain height
integer:: i,j,jts1,jte1,its1,ite1,jfts1,jfte1,ifts1,ifte1

! terrain height

    jts1=max(jts-1,jds) ! lower loop limit by one less when at end of domain
    its1=max(its-1,ids) ! ASSUMES THE HALO IS THERE if patch != domain
    jte1=min(jte+1,jde) 
    ite1=min(ite+1,ide)
    do j = jts1,jte1
        do i = its1,ite1 
            ! copy to local array
            za(i,j)=zs(i,j)           
        enddo
    enddo

    call continue_at_boundary(1,1, & ! do x direction or y direction
    its-2,ite+2,jts-2,jte+2,           &                ! memory dims
    ids,ide,jds,jde, &            ! domain dims - winds defined up to +1
    its1,ite1,jts1,jte1, &                ! tile dims
    za)                               ! array

    ! interpolate to tile plus strip along domain boundary if at boundary
    jfts1=snode(jfts,jfds,-1) ! lower loop limit by one less when at end of domain
    ifts1=snode(ifts,ifds,-1)
    jfte1=snode(jfte,jfde,+1) 
    ifte1=snode(ifte,ifde,+1)
                     
    call interpolate_2d(  &
        its-2,ite+2,jts-2,jte+2, & ! memory dims atm grid tile
        its1-1,ite1+1,jts1-1,jte1+1, & ! where atm grid values set
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ifts1,ifte1,jfts1,jfte1,  & ! dimensions fire grid tile
        ir,jr,                  & ! refinement ratio
        real(ids),real(jds),ifds+(ir+1)*.5,jfds+(jr+1)*.5, & ! line up by lower left corner of domain
        za,                     & ! in atm grid     
        zsf)                      ! out fire grid

end subroutine interpolate_z2fire
!
!*****************************
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
    write(msg,1)s,ids,ide,ifds,ifde,ir
1   format('module_fr_sfire_driver: incompatible bounds ',a,' atm ',i5,':',i5,' fire ',i5,':',i5,' ratio ',i3)    
    call crash(msg)
endif
end subroutine check_fmesh

!
!*****************************
!

end module module_fr_sfire_driver
