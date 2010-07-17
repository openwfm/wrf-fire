module module_fr_sfire_driver_wrf
! wrf specific subroutines that need to be called from  the driver

use module_fr_sfire_util
use module_dm

implicit none

contains


subroutine fire_ignition_convert (config_flags,fire_max_ignitions,fire_ignition_longlat, &
    fire_ignition_start_x,fire_ignition_start_y,fire_ignition_end_x,fire_ignition_end_y, &
    fire_ignition_radius,fire_ignition_start_time,fire_ignition_end_time,fire_num_ignitions, &
    unit_fxlong,unit_fxlat)
    use module_model_constants, only: reradius,pi2
    USE module_configure
    implicit none
! create ignition arrays from scalar flags
!*** arguments
    TYPE (grid_config_rec_type) , INTENT(IN)          :: config_flags
    integer, intent(in)::fire_max_ignitions
    real, dimension(fire_max_ignitions), intent(out):: &
        fire_ignition_start_x,fire_ignition_start_y,fire_ignition_end_x,fire_ignition_end_y, &
        fire_ignition_radius,fire_ignition_start_time,fire_ignition_end_time
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
        fire_ignition_end_x(5)=config_flags%fire_ignition_end_x5
        fire_ignition_end_y(5)=config_flags%fire_ignition_end_y5
    endif
    if(real)then
        ! use values from _long and _lat
        fire_ignition_longlat=1
        fire_ignition_start_x(1)=config_flags%fire_ignition_start_lon1
        fire_ignition_start_y(1)=config_flags%fire_ignition_start_lat1
        fire_ignition_end_x(1)=config_flags%fire_ignition_end_lon1
        fire_ignition_end_y(1)=config_flags%fire_ignition_end_lat1
        fire_ignition_start_x(2)=config_flags%fire_ignition_start_lon2
        fire_ignition_start_y(2)=config_flags%fire_ignition_start_lat2
        fire_ignition_end_x(2)=config_flags%fire_ignition_end_lon2
        fire_ignition_end_y(2)=config_flags%fire_ignition_end_lat2
        fire_ignition_start_x(3)=config_flags%fire_ignition_start_lon3
        fire_ignition_start_y(3)=config_flags%fire_ignition_start_lat3
        fire_ignition_end_x(3)=config_flags%fire_ignition_end_lon3
        fire_ignition_end_y(3)=config_flags%fire_ignition_end_lat3
        fire_ignition_start_x(4)=config_flags%fire_ignition_start_lon4
        fire_ignition_start_y(4)=config_flags%fire_ignition_start_lat4
        fire_ignition_end_x(4)=config_flags%fire_ignition_end_lon4
        fire_ignition_end_y(4)=config_flags%fire_ignition_end_lat4
        fire_ignition_start_x(5)=config_flags%fire_ignition_start_lon5
        fire_ignition_start_y(5)=config_flags%fire_ignition_start_lat5
        fire_ignition_end_x(5)=config_flags%fire_ignition_end_lon5
        fire_ignition_end_y(5)=config_flags%fire_ignition_end_lat5
    endif
    ! common to both cases
        fire_ignition_radius(1)=config_flags%fire_ignition_radius1 
        fire_ignition_start_time(1)=config_flags%fire_ignition_start_time1 
        fire_ignition_end_time(1)=config_flags%fire_ignition_end_time1 
        fire_ignition_radius(2)=config_flags%fire_ignition_radius2 
        fire_ignition_start_time(2)=config_flags%fire_ignition_start_time2 
        fire_ignition_end_time(2)=config_flags%fire_ignition_end_time2 
        fire_ignition_radius(3)=config_flags%fire_ignition_radius3 
        fire_ignition_start_time(3)=config_flags%fire_ignition_start_time3 
        fire_ignition_end_time(3)=config_flags%fire_ignition_end_time3 
        fire_ignition_radius(4)=config_flags%fire_ignition_radius4 
        fire_ignition_start_time(4)=config_flags%fire_ignition_start_time4 
        fire_ignition_end_time(4)=config_flags%fire_ignition_end_time4 
        fire_ignition_radius(5)=config_flags%fire_ignition_radius5 
        fire_ignition_start_time(5)=config_flags%fire_ignition_start_time5
        fire_ignition_end_time(5)=config_flags%fire_ignition_end_time5

    ! 
        fire_num_ignitions=0      
        do i=1,min(5,config_flags%fire_num_ignitions)
            ! count the ignitions 
            if(fire_ignition_radius(i).gt.0.)fire_num_ignitions=i
            ! expand ignition data given as zero
            if(fire_ignition_end_x(i).eq.0.)fire_ignition_end_x(i)=fire_ignition_start_x(i)
            if(fire_ignition_end_y(i).eq.0.)fire_ignition_end_y(i)=fire_ignition_start_y(i)
            if(fire_ignition_end_time(i).eq.0.)fire_ignition_end_time(i)=fire_ignition_start_time(i)
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
!module_fr_sfire_driver%%interpolate_atm2fire

subroutine interpolate_atm2fire(id,               & ! for debug output, <= 0 no output
    ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
    ims,ime, kms,kme, jms,jme,                    &
    ips,ipe,jps,jpe,                              &
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
    ips,ipe,jps,jpe,                              &
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
    call continue_at_boundary(1,0,0., & ! do x direction or y direction
    TDIMS,           &                ! memory dims
    ids,ide+1,jds,jde+1, &            ! domain dims - winds defined up to +1
    ips,ipe+1,jps,jpe+1, &            ! patch dims - winds defined up to +1
    its,ite+1,jts,jte+1, &                ! tile dims
    va)                               ! array

    call continue_at_boundary(0,1,0., & ! do x direction or y direction
    TDIMS,           &                ! memory dims
    ids,ide+1,jds,jde+1, &            ! domain dims - winds defined up to +1
    ips,ipe+1,jps,jpe+1, &            ! patch dims - winds defined up to +1
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
!     | F | F | F | F |   Winds are given at the midpoints of the sides of the atmosphere grid,
!     ua------z-------|   interpolated to midpoints of the cells of the fine fire grid F.
!     | F | F | F | F |   This is (1,1) cell of atmosphere grid, and [*] is the (1,1) cell of the fire grid.
!     |---------------|   ua(1,1) <--> uf(0.5,2.5)
!     | * | F | F | F |   va(1,1) <--> vf(2.5,0.5)
!      -------va------    za(1,1) <--> zf(2.5,2.5)
!
!   ^ x2
!   |  --------va(1,2)---------
!   | |            |           |   Example of atmospheric and fire grid with
!   | |            |           |   ir=jr=1.
!   | |          za,zf         |   Winds are given at the midpoints of the sides of the atmosphere grid,
!   | ua(1,1)----uf,vf-----ua(2,1) interpolated to midpoints of the cells of the (the same) fire grid 
!   | |           (1,1)        |   ua(1,1) <--> uf(0.5,1) 
!   | |            |           |   va(1,1) <--> vf(1,0.5) 
!   | |            |           |   za(1,1) <--> zf(1,1)
!   |  --------va(1,1)---------
!   |--------------------> x1 
!
! Meshes are aligned by the lower left cell of the domain. Then in the above figure
! u = node with the ua component of the wind at (ids,jds), midpoint of side
! v = node with the va component of the wind at (ids,jds), midpoint of side
! * = fire grid node at (ifds,jfds)
! z = node with height, midpoint of cell
! 
! ua(ids,jds)=uf(ifds-0.5,jfds+jr*0.5-0.5)         = uf(ifds-0.5,jfds+(jr-1)*0.5)
! va(ids,jds)=vf(ifds+ir*0.5-0.5,jfds-0.5)         = vf(ifds+(ir-1)*0.5,jfds-0.5)
! za(ids,jds)=zf(ifds+ir*0.5-0.5,jfds+jr*0.5-0.5)  = zf(ifds+(ir-1)*0.5,jfds+(jr-1)*0.5)
    
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
        real(ids),real(jds),ifds-0.5,jfds+(jr-1)*0.5, & ! line up by lower left corner of domain
        ua,                     & ! in atm grid     
        uf)                      ! out fire grid

    call interpolate_2d(  &
        TDIMS,                  & ! memory dims atm grid tile
        TDIMS,                  & ! where atm grid values set
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ifts1,ifte1,jfts1,jfte1,& ! dimensions on the fire grid to interpolate to
        ir,jr,                  & ! refinement ratio
        real(ids),real(jds),ifds+(ir-1)*0.5,jfds-0.5, & ! line up by lower left corner of domain
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
    ips,ipe,jps,jpe,                              &
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
    ips,ipe,jps,jpe,                              &
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

    call continue_at_boundary(1,1,0., & ! do x direction or y direction
    its-2,ite+2,jts-2,jte+2,           &                ! memory dims
    ids,ide,jds,jde, &            ! domain dims - winds defined up to +1
    ips,ipe,jps,jpe, &            ! patch dims - winds defined up to +1
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
        real(ids),real(jds),ifds+(ir-1)*0.5,jfds+(jr-1)*0.5, & ! line up by lower left corner of domain
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
fire_upwind_split       = config_flags%fire_upwind_split 
fire_viscosity          = config_flags%fire_viscosity 
fire_lfn_ext_up         = config_flags%fire_lfn_ext_up 
fire_test_steps         = config_flags%fire_test_steps 
fire_topo_from_atm      = config_flags%fire_topo_from_atm
fire_advection          = config_flags%fire_advection

end subroutine set_flags

end module module_fr_sfire_driver_wrf
