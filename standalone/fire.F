module module_fire_standalone

use module_fr_sfire_util, only: message,crash
use module_fr_sfire_driver, only: fire_ignition_convert,set_fp_from_grid
use module_fr_sfire_core, only: ignition_type,fire_max_ignitions
use module_fr_sfire_phys, only: fire_params
use module_domain, only: domain
use module_configure, only: grid_config_rec_type,read_namelist
use wrf_netcdf, only : grid_info, set_fire_info_from_file, &
                       create_output_file,write_vars,output_vars_fire, &
                       input_vars_fire,read_fire_vars
implicit none

contains 

subroutine sub_main

!*** purpose: standalone driver with compatible files to WRF-Fire

implicit none

!*** local

! arguments to SFIRE

type(domain)::grid          ! all: state+inputs+outputs, compatible with wrf
TYPE (grid_config_rec_type):: config_flags ! the namelist
integer::  &                ! fire mesh dimensions
    ifds,ifde,jfds,jfde, &  ! the physical domain
    ifps,ifpe,jfps,jfpe, &  ! patch - assigned to one process. Here the same as domain.
    ifms,ifme,jfms,jfme     ! memory allocated, needs a strip around the patch

! I/O interface
character(len=*),parameter::inputfile='fire_input.nc'
character(len=*),parameter::outputfile='fire_output.nc'
type(output_vars_fire)::output                ! output arrays
type(input_vars_fire)::input                  ! input arrays_fire

! other derived types
type(grid_info)::info                    ! dimensions, grid controls

! scalars
integer:: nsteps,itimestep
integer::nhalo=5
double precision:: dt, duration_s  ! may need more accurate time computation to get the number of timesteps right
real:: time,time_step_start, dts
logical::do_ouput
TYPE(ignition_type) :: ignition
type(fire_params)::fp

!*** executable

call read_namelist(config_flags)           ! read flags from namelist.input
call set_fire_info_from_file(inputfile,info)     ! get dimensions

! copy pointers to grid fields, to pass to the spread rate calculation
call set_fp_from_grid(grid,fp)

! get ignition data
call fire_ignition_convert (config_flags,ignition)

! set dimensions
ifds=1
ifde=info%nfirex
jfds=1
jfde=info%nfirey
ifms=ifds-nhalo
ifme=ifde+nhalo
jfms=ifds-nhalo
jfme=ifde+nhalo
ifps=1
ifpe=ifde
jfps=1
jfpe=jfde

write(6,2)'fire domain dimensions       ',ifds,ifde,jfds,jfde
write(6,2)'fire memory dimensions       ',ifms,ifme,jfms,jfme
2 format(a,6i6)

! allocate

! inputs
call allocate2d(grid%uf,ifms,ifme,jfms,jfme,'uf')              ! fire winds
call allocate2d(grid%vf,ifms,ifme,jfms,jfme,'vf')              ! fire winds
call allocate2d(grid%zsf,ifms,ifme,jfms,jfme,'zsf')             ! terrain height
call allocate2d(grid%dzdxf,ifms,ifme,jfms,jfme,'dzdxf')           ! terrain grad
call allocate2d(grid%dzdyf,ifms,ifme,jfms,jfme,'dzdyf')           ! terrain grad
call allocate2d(grid%fxlong,ifms,ifme,jfms,jfme,'fxlong')          ! 
call allocate2d(grid%fxlat,ifms,ifme,jfms,jfme,'fxlat')           !
call allocate2d(grid%nfuel_cat,ifms,ifme,jfms,jfme,'nfuel_cat')          ! 

! state
call allocate2d(grid%bbb,ifms,ifme,jfms,jfme,'bbb')             ! spread formula coeff
call allocate2d(grid%betafl,ifms,ifme,jfms,jfme,'betafl')          ! spread formula coeff
call allocate2d(grid%phiwc,ifms,ifme,jfms,jfme,'phiwc')           ! spread formula coeff
call allocate2d(grid%r_0,ifms,ifme,jfms,jfme,'r_0')             ! spread formula coeff
call allocate2d(grid%fgip,ifms,ifme,jfms,jfme,'fgip')            ! spread formula coeff
call allocate2d(grid%ischap,ifms,ifme,jfms,jfme,'ischap')          ! spread formula coeff
call allocate2d(grid%fuel_time,ifms,ifme,jfms,jfme,'fuel_time')        ! 
call allocate2d(grid%lfn,ifms,ifme,jfms,jfme,'lfn') 
call allocate2d(grid%tign_g,ifms,ifme,jfms,jfme,'tign') 
call allocate2d(grid%fuel_frac,ifms,ifme,jfms,jfme,'fuel_frac') 
call allocate2d(grid%lfn_out,ifms,ifme,jfms,jfme,'lfn_out') 

! outputs
call allocate2d(grid%fire_area,ifms,ifme,jfms,jfme,'fire_area') 
call allocate2d(grid%ros,ifms,ifme,jfms,jfme,'ros') 
call allocate2d(grid%flineint,ifms,ifme,jfms,jfme,'flineint') 
call allocate2d(grid%flineint2,ifms,ifme,jfms,jfme,'flineint2') 
call allocate2d(grid%fgrnhfx,ifms,ifme,jfms,jfme,'fgrnhfx')          ! 
call allocate2d(grid%fgrnqfx,ifms,ifme,jfms,jfme,'fgrnqfx')          ! 
call allocate2d(grid%fcanhfx,ifms,ifme,jfms,jfme,'fcanhfx')          ! 
call allocate2d(grid%fcanqfx,ifms,ifme,jfms,jfme,'fcanqfx')          ! 
call allocate2d(grid%f_ros,ifms,ifme,jfms,jfme,'f_ros')              ! 
call allocate2d(grid%f_ros0,ifms,ifme,jfms,jfme,'f_ros0')            ! 
call allocate2d(grid%f_rosx,ifms,ifme,jfms,jfme,'f_rosx')            ! 
call allocate2d(grid%f_rosy,ifms,ifme,jfms,jfme,'f_rosy')            ! 
call allocate2d(grid%f_lineint,ifms,ifme,jfms,jfme,'f_lineint')      ! 
call allocate2d(grid%f_lineint2,ifms,ifme,jfms,jfme,'f_lineint2')    ! 
call allocate2d(grid%f_int,ifms,ifme,jfms,jfme,'f_int')              ! 

call read_fire_vars(inputfile,info,1,grid)

! time control
! NOTE: dt in the netcdf input file as returned in info%dt is WRONG !!
dt=config_flags%time_step
if(config_flags%time_step_fract_den.ne.0)then
  dt=dt+dble(config_flags%time_step_fract_num)/dble(config_flags%time_step_fract_den)
endif
duration_s = config_flags%run_seconds           &
           + 60d0*(config_flags%run_minutes     &
           + 60d0*(config_flags%run_hours       &
           + 24d0*(config_flags%run_days)))       
nsteps = nint( duration_s / dt ) ! number of time steps

! divide up for shared memory parallel execution
!!call set_tiles(1,1,ips,ipe,jps,jpe,grid%num_tiles,grid%i_start,grid%i_end,grid%j_start,grid%j_end)

! set the scalars in grid type
grid%dt = dt
grid%itimestep=0
grid%u_frame=0.
grid%v_frame=0.

! start output file
call create_output_file(outputfile,info)

! initialize model
   time_step_start  =0.
   dts=dt
!call sfire_driver_em ( grid , config_flags                          &
!            ,time_step_start,dts                                    &
!            ,1,2,0                                                 &
!            ,ids,ide, kds,kde, jds,jde                              &
!            ,ims,ime, kms,kme, jms,jme                              &
!            ,ips,ipe, kps,kpe, jps,jpe                              &
!            ,ifds,ifde, jfds,jfde                                   &
!            ,ifms,ifme, jfms,jfme                                   &
!            ,ifps,ifpe, jfps,jfpe )

do itimestep=1,nsteps
! run  model
   grid%itimestep = itimestep
   time_step_start = itimestep*dt
   dts=dt
!   call sfire_driver_em ( grid , config_flags                          &
!            ,time_step_start,dts                                    &
!            ,3,6,0                                                  &
!            ,ids,ide, kds,kde, jds,jde                              &
!            ,ims,ime, kms,kme, jms,jme                              &
!            ,ips,ipe, kps,kpe, jps,jpe                              &
!            ,ifds,ifde, jfds,jfde                                   &
!            ,ifms,ifme, jfms,jfme                                   &
!            ,ifps,ifpe, jfps,jfpe )
    
    if(itimestep.le.10.or.mod(itimestep,10).eq.0)then
        time = dt*itimestep
        output%lfn=>grid%lfn(ifps:ifpe,jfps:jfpe)
        output%tign=>grid%tign_g(ifps:ifpe,jfps:jfpe)
        output%fgrnhfx=>grid%fgrnhfx(ifps:ifpe,jfps:jfpe)
        call write_vars(outputfile,output,time)
    endif
enddo

end subroutine sub_main

!subroutine sfire_model (                    &
!    id,                                     & ! unique number for prints and debug
!    ifun,                                   & ! what to do see below
!    restart,                                & ! use existing state
!    num_ignitions,                          & ! number of ignitions before advancing
!    ifuelread,nfuel_cat0,                   & ! initialize fuel categories
!    ifds,ifde,jfds,jfde,                    & ! fire domain dims - the whole domain
!    ifms,ifme,jfms,jfme,                    & ! fire memory dims - how declared
!    ifps,ifpe,jfps,jfpe,                    & ! patch - nodes owned by this process
!    ifts,ifte,jfts,jfte,                    & ! fire tile dims  - this thread
!    time_start,dt,                          & ! time and increment
!    fdx,fdy,                                & ! fire mesh spacing,
!    ignition_line,                          & ! small array of ignition line descriptions
!    ignitions_done,ignited_tile,            &
!    coord_xf,coord_yf,unit_xf,unit_yf,      & ! fire mesh coordinates
!    lfn,lfn_out,tign,fuel_frac,fire_area,   & ! state: level function, ign time, fuel left, area burning
!    grnhfx,grnqfx,                          & ! output: heat fluxes
!    ros,flineint,flineint2,                 & ! diagnostic variables
!    f_ros0,f_rosx,f_rosy,f_ros,             & ! fire risk spread
!    f_int,f_lineint,f_lineint2,             & ! fire risk intensities
!    nfuel_cat,                              & ! fuel data per point
!    fuel_time,fwh,fz0,                      & ! save derived internal data
!    fp &
!)


!subroutine model_driver(grid,config_flags)

!
!******************************
!

subroutine set_tiles(itiles,jtiles,ips,ipe,jps,jpe,num_tiles,i_start,i_end,j_start,j_end)
!*** set tiles for standalone/testing
implicit none
!*** arguments
integer,intent(in)::itiles,jtiles,ips,ipe,jps,jpe
integer,intent(out)::num_tiles
integer,intent(out),dimension(itiles*jtiles)::i_start,i_end,j_start,j_end
!*** local
integer::i,j,istep,jstep,ij
character(len=128)::msg
write(msg,1)'patch',ips,':',ipe,jps,':',jpe
1 format(a,5x,i6,a,2i6,a,i6)
call message(msg,level=-1)
!if(ips.ge.ipe.or.jps.ge.jpe)call crash('bad domain bounds')
!num_tiles=itiles*jtiles
!istep=(ipe-ips+itiles)/itiles
!jstep=(jpe-jps+jtiles)/jtiles
!do i=1,itiles
!    do j=1,jtiles
!        ij=j+(i-1)*jtiles
!        i_start(ij)=min(ipe,ips+(i-1)*istep)
!        i_end(ij)  =min(ipe,ips+(i  )*istep-1)
!        j_start(ij)=min(jpe,jps+(j-1)*jstep)
!        j_end(ij)  =min(jpe,jps+(j  )*jstep-1)
!    enddo
!enddo
!call check_tiles(ips,ipe,jps,jpe,num_tiles,i_start,i_end,j_start,j_end)
end subroutine set_tiles


subroutine check_tiles(ips,ipe,jps,jpe,num_tiles,i_start,i_end,j_start,j_end)
implicit none
!*** purpose: check if tiles fit
!*** arguments
integer,intent(in)::ips,ipe,jps,jpe,num_tiles
integer,intent(in),dimension(num_tiles)::i_start,i_end,j_start,j_end
!*** local
character(len=128)::msg
integer:: ij,ie
!*** executable
if(num_tiles.lt.1)call crash('check_tiles: need at least one tile')
ie=0
do ij=1,num_tiles
    if(i_start(ij).lt.ips.or.i_end(ij).gt.ipe &
    .or.j_start(ij).lt.jps.or.j_end(ij).gt.jpe)then
        write(msg,1)'patch',ips,':',ipe,jps,':',jpe
1       format(a,5x,i6,a,2i6,a,i6)
        call message(msg,level=-1)
        write(msg,2)'tile',ij,i_start(ij),':',i_end(ij),j_start(ij),':',j_end(ij)
2       format(a,2i6,a,2i6,a,i6)
        call message(msg,level=-1)
        call crash('bad tile bounds')
    endif
enddo
end subroutine check_tiles


subroutine allocate2d(p,ims,ime,jms,jme,s) 
!*** allocate a pointer with error checking and initialization
implicit none
!*** arguments
real, pointer, intent(out), dimension(:,:)::p
integer, intent(in):: ims,ime,jms,jme
character(len=*),intent(in)::s
!*** local
integer::err
!*** executable
write(6,1) ims,ime,jms,jme,trim(s)
if(associated(p))call crash('already allocated')
1 format('allocate2d',2(1x,i6,' :',i6),1x,a)
allocate(p(ims:ime,jms:jme),stat=err)
if(err.ne.0)then
   write(6,1)ims,ime,jms,jme,trim(s)
   call crash('memory allocation failed')
endif
p=0.
end subroutine allocate2d

subroutine allocate3d(p,ims,ime,jms,jme,kms,kme,s) 
!*** allocate a pointer with error checking and initialization
implicit none
!*** arguments
real, pointer, intent(out), dimension(:,:,:)::p
integer, intent(in):: ims,ime,jms,jme,kms,kme
character(len=*),intent(in)::s
!*** local
integer::err
!*** executable
write(6,1) ims,ime,jms,jme,kms,kme,trim(s)
1 format('allocate3d',3(1x,i6,' :',i6),1x,a)
if(associated(p))call crash('already allocated')
allocate(p(ims:ime,jms:jme,kms:kme),stat=err)
if(err.ne.0)then
   write(6,1)ims,ime,jms,jme,kms,kme,trim(s)
   call crash('memory allocation failed')
endif
p=0.
end subroutine allocate3d

end module module_fire_standalone

!
!******************************
!


program fire
use module_fire_standalone, only: sub_main
call  sub_main
end program fire
