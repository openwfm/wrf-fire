! the main program is at the end because of fortran limitations

module module_model_test

use module_configure
use module_fr_sfire_util
use module_fr_sfire_model
use module_fr_sfire_phys

contains

subroutine main_sub
use wrf_netcdf, only : grid_info, set_grid_from_file, inputfile, &
                       create_output_file,write_vars,grid_info,output_vars, &
                       input_vars,read_vars
implicit none

!*** declarations

real:: time_start     ! time control

integer:: nx,ny, msteps  ! problem dimension, in cells, number of steps
real:: fdx,fdy, dt       ! fire mesh spacing (m), time step (s)
integer::  &             ! dimensions
    ifds,ifde,jfds,jfde, &  ! the physical domain
    ifps,ifpe,jfps,jfpe, &  ! patch - assigned to one process. Here the same as domain.
    ifms,ifme,jfms,jfme     ! memory allocated, needs a strip around the patch
integer:: sm,sn
double precision::duration_s


! state arrays
real, dimension(:,:), pointer :: lfn,tign,fuel_frac,fire_area, grnhfx,grnqfx,lfn_out,ros
real, dimension(:,:), pointer:: fuel_time, coord_xf,coord_yf,nfuel_cat
type(fire_params)::fp   ! arrays passed to spread rate computation

! ignition
real:: unit_xf,unit_yf    ! units to specify ignintion location in
integer::  num_ignitions
integer, parameter :: max_ignitions=10
real, dimension(max_ignitions) :: ignition_start_x,ignition_start_y, & ! from namelist.input
    ignition_end_x,ignition_end_y,ignition_radius,ignition_start_time,ignition_end_time
integer:: ignited_tile(max_ignitions),ignitions_done

! parallel execution control
logical::need_lfn_update ! parallel flag - not used here
integer, parameter::max_tiles=10
integer::num_tiles,ij,ifts,ifte,jfts,jfte
integer, dimension(max_tiles)::if_start,if_end,jf_start,jf_end

! fuel controls
integer:: fire_fuel_read,fire_fuel_cat

! loop control
integer:: i,j,istep,ifun,ifun_start

! derived types for I/O interface
character(len=*),parameter::filename='fire_output.nc'
type(output_vars)::output                ! output arrays
type(input_vars)::input                  ! input arrays
type(grid_info)::grid                    ! dimensions, grid controls
type(grid_config_rec_type)::config_flags ! for the namelist

!*** executable

! kill output files
fire_print_file=0

call read_namelist(config_flags)
call set_grid_from_file(inputfile,grid)

nx=grid%nfirex
ny=grid%nfirey
fdx=grid%dx/grid%sr_x
fdy=grid%dy/grid%sr_y

! time control
dt=grid%dt
duration_s = config_flags%run_seconds           &
           + 60d0*(config_flags%run_minutes     &
           + 60d0*(config_flags%run_hours       &
           + 24d0*(config_flags%run_days)))       
msteps = duration_s / dt  ! number of time steps

print *,'nx=',nx,' ny=',ny,' msteps=',msteps,' fdx=',fdx,' fdy=',fdy,' dt=',dt

! set domain size
ifds=1
ifde=nx
jfds=1
jfde=ny
ifms=0
ifme=nx+1
jfms=0
jfme=ny+1
ifps=1
ifpe=nx
jfps=1
jfpe=ny

! coefficoent arrays to be passed to spread formulas, constant after initialized
allocate(fp%vx(ifms:ifme,jfms:jfme))              ! fire winds
allocate(fp%vy(ifms:ifme,jfms:jfme))              ! fire winds
allocate(fp%zsf(ifms:ifme,jfms:jfme))             ! terrain height
allocate(fp%dzdxf(ifms:ifme,jfms:jfme))           ! terrain grad
allocate(fp%dzdyf(ifms:ifme,jfms:jfme))           ! terrain grad
allocate(fp%bbb(ifms:ifme,jfms:jfme))             ! spread formula coeff
allocate(fp%betafl(ifms:ifme,jfms:jfme))          ! spread formula coeff
allocate(fp%phiwc(ifms:ifme,jfms:jfme))           ! spread formula coeff
allocate(fp%r_0(ifms:ifme,jfms:jfme))             ! spread formula coeff
allocate(fp%fgip(ifms:ifme,jfms:jfme))            ! spread formula coeff
allocate(fp%ischap(ifms:ifme,jfms:jfme))          ! spread formula coeff

! other coefficient arrays
allocate(fuel_time(ifms:ifme,jfms:jfme)) 
allocate(coord_xf(ifms:ifme,jfms:jfme)) 
allocate(coord_yf(ifms:ifme,jfms:jfme)) 
allocate(nfuel_cat(ifms:ifme,jfms:jfme)) 

! model state 
allocate(lfn(ifms:ifme,jfms:jfme)) 
allocate(tign(ifms:ifme,jfms:jfme)) 
allocate(fuel_frac(ifms:ifme,jfms:jfme)) 
allocate(fire_area(ifms:ifme,jfms:jfme)) 
allocate(grnhfx(ifms:ifme,jfms:jfme)) 
allocate(grnqfx(ifms:ifme,jfms:jfme)) 

! misc variable arrays
allocate(lfn_out(ifms:ifme,jfms:jfme)) 
allocate(ros(ifms:ifme,jfms:jfme)) 

! divide up for parallel execution
call set_tiles(2,2,ifps,ifpe,jfps,jfpe,num_tiles,if_start,if_end,jf_start,jf_end)

print *,'mesh size in cells: ',ifps,ifpe,jfps,jfpe
print *,'array allocation:   ',ifms,ifme,jfms,jfme

! start output file
call create_output_file(filename,grid)

! read input file
input%nfuel_cat=>nfuel_cat(ifps:ifpe,jfps:jfpe)
input%zsf=>fp%zsf(ifps:ifpe,jfps:jfpe)
input%dzdxf=>fp%dzdxf(ifps:ifpe,jfps:jfpe)
input%dzdyf=>fp%dzdyf(ifps:ifpe,jfps:jfpe)
input%vx=>fp%vx(ifps:ifpe,jfps:jfpe)
input%vy=>fp%vy(ifps:ifpe,jfps:jfpe)

call read_vars(inputfile,input,grid)

sm=ifpe-ifps+1
sn=jfpe-jfps+1

time_start=0             ! we count time from the beginning of the simulation
fire_fuel_read=config_flags%fire_fuel_read
fire_fuel_cat=config_flags%fire_fuel_cat

num_ignitions=2
ignition_start_x(1)=0.5*fdx*(ifde-ifds)
ignition_start_y(1)=0.5*fdy*(jfde-jfds)
ignition_end_x(1)=0.5*fdx*(ifde-ifds)*0.9999999
ignition_end_y(1)=0.5*fdy*(jfde-jfds)*1.0000001
ignition_radius(1) = 0.5*max(5.0,6*max(fdx,fdy))
ignition_start_time(1)=1
ignition_end_time(1)=1
ignition_start_x(2)=1000
ignition_start_y(2)=500
ignition_end_x(2)=1500
ignition_end_y(2)=1500
! at least 6 by 6 cells but no less than 5 m
ignition_radius(2) = 0.5*max(5.0,6*max(fdx,fdy))
ignition_start_time(2)=2
ignition_end_time(2)=2

unit_xf=1
unit_yf=1

do istep=1,msteps
    ifun_start=1
    if(istep.ne.1)ifun_start=3
    do ifun=ifun_start,6
!OMP    PARALLEL DO PRIVATE(ij,ifts,ifte,jfts,jfte)        
        do ij=1,num_tiles
            ifts= if_start(ij)          
            ifte= if_end(ij)
            jfts= jf_start(ij)
            jfte= jf_end(ij)


            call set_ideal_coord( fdx,fdy, &
                ifds,ifde,jfds,jfde,  &
                ifms,ifme,jfms,jfme,  &
                ifts,ifte,jfts,jfte,  &
                coord_xf,coord_yf     &
            )

            call   sfire_model (                        &
                10*istep+ij,                            & ! id
                ifun,                                   &
                .false.,                                & ! restart
                need_lfn_update,                        &
                num_ignitions,                          & 
                fire_fuel_read,fire_fuel_cat,           &
                ifds,ifde,jfds,jfde,                    & ! fire domain dims - the whole domain
                ifms,ifme,jfms,jfme,                    & ! fire memory dims - how declared
                ifds,ifde,jfds,jfde,                    & ! patch=domain 
                ifts,ifte,jfts,jfte,                    & ! fire tile dims  - this thread
                time_start,dt,                          & ! time and increment
                fdx,fdy,                                & ! fire mesh spacing
                ignition_start_x,ignition_start_y,      &
                ignition_end_x,ignition_end_y,          &  
                ignition_radius,                        &
                ignition_start_time,                    &
                ignition_end_time,                      &
                ignitions_done,ignited_tile,            &
                coord_xf,coord_yf,unit_xf,unit_yf,      & ! fire mesh coordinates
                lfn,lfn_out,tign,fuel_frac,fire_area,   & ! state: level function, ign time, fuel left
                grnhfx,grnqfx,                          & ! output: heat fluxes
                ros,                                    & ! output: rate of spread
                nfuel_cat,                              & ! fuel data per point 
                fuel_time,                              & ! save derived internal data
                fp                                      & ! args passed to ros computation        
            )
        enddo 
        
    enddo !OMP PARALLEL
    
    if(istep.le.10.or.mod(istep,10).eq.0)then
        output%lfn=>lfn(ifps:ifpe,jfps:jfpe)
        output%tign=>tign(ifps:ifpe,jfps:jfpe)
        output%vx=>fp%vx(ifps:ifpe,jfps:jfpe)
        output%vy=>fp%vy(ifps:ifpe,jfps:jfpe)
        output%grnhfx=>grnhfx(ifps:ifpe,jfps:jfpe)
        call write_vars(filename,output,time_start)
    endif
    print *,'test_main: step ',istep,' of ',msteps,' time ',time_start
    time_start=time_start+dt 
enddo

!close(1)

end subroutine main_sub

!
!******************************
!

subroutine set_tiles(itiles,jtiles,ids,ide,jds,jde,num_tiles,i_start,i_end,j_start,j_end)
!*** set tiles for standalone/testing
implicit none
!*** arguments
integer,intent(in)::itiles,jtiles,ids,ide,jds,jde
integer,intent(out)::num_tiles
integer,intent(out),dimension(itiles*jtiles)::i_start,i_end,j_start,j_end
!*** local
integer::i,j,istep,jstep,ij
num_tiles=itiles*jtiles
istep=(ide-ids+itiles)/itiles
jstep=(jde-jds+jtiles)/jtiles
do i=1,itiles
    do j=1,jtiles
        ij=j+(i-1)*jtiles
        i_start(ij)=min(ide,ids+(i-1)*istep)
        i_end(ij)  =min(ide,ids+(i  )*istep-1)
        j_start(ij)=min(jde,jds+(j-1)*jstep)
        j_end(ij)  =min(jde,jds+(j  )*jstep-1)
    enddo
enddo
call check_tiles(ids,ide,jds,jde,num_tiles,i_start,i_end,j_start,j_end)
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
if (num_tiles.eq.1) then
    if(i_start(1).ne.ips.or.i_end(1).ne.ipe.or.j_start(1).ne.jps.or.j_end(1).ne.jpe)ie=1
else
    do ij=1,num_tiles
        if(i_start(ij).lt.ips.or.i_end(ij).gt.ipe &
        .or.j_start(ij).lt.jps.or.j_end(ij).gt.jpe)ie=ij
    enddo
endif
if(ie.ne.0)then        
    write(msg,*)'bad tile ',ie
    call message(msg)
    write(msg,*)'patch dimensions:',ips,ipe,jps,jpe
    call message(msg)
    do ij=1,num_tiles
        write(msg,*)'tile',ij,i_start(ij),i_end(ij),j_start(ij),j_end(ij)
        call message(msg)
    enddo
    call crash('bad tile bounds')
endif
end subroutine check_tiles


end module module_model_test

!
!******************************
!


program model_test_main
use module_model_test
call  main_sub
end program model_test_main
