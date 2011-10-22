! A simple program to initialize a test case for wrf-fire standalone driver.  
! It will create a netcdf file called fire_input.nc containing initial
! conditions for the fire simulation, somewhat compatible with wrfinput_d01
! for WRF, created by ideal.exe or real.exe

program model_test_init

use module_configure
use wrf_netcdf
use module_fr_sfire_util, only : crash
implicit none

type(grid_info)::grid
type(input_vars_fire)::input
integer::ierr,i,j
real,parameter::wind=10.,alpha=0.0
type(grid_config_rec_type)::config_flags ! for the namelist

! read namelist parameters
call read_namelist(config_flags)

! get grid information
grid%natmx=config_flags%e_we-config_flags%s_we+1
grid%natmy=config_flags%e_sn-config_flags%s_sn+1
grid%natmz=config_flags%e_vert-config_flags%s_vert+1
grid%sr_x=config_flags%sr_x
grid%sr_y=config_flags%sr_y
grid%nfirex=grid%natmx*config_flags%sr_x
grid%nfirey=grid%natmy*config_flags%sr_y
grid%dx=config_flags%dx
grid%dy=config_flags%dy
grid%dt=real(config_flags%time_step)+real(config_flags%time_step_fract_num)/real(config_flags%time_step_fract_den)

! allocate space for input variables
allocate(input%nfuel_cat(grid%nfirex,grid%nfirey))
allocate(input%dzdxf(grid%nfirex,grid%nfirey)) 
allocate(input%dzdyf(grid%nfirex,grid%nfirey))
allocate(input%uah(grid%natmx+1,grid%natmy))
allocate(input%vah(grid%natmx,grid%natmy+1))
allocate(input%zsf(grid%nfirex,grid%nfirey))

! initialize input variables
do j=1,grid%nfirey
  do i=1,grid%nfirex
    input%zsf(i,j)=1000.
    input%dzdxf(i,j)=0.
    input%dzdyf(i,j)=0.
    input%nfuel_cat(i,j)=3
  enddo
enddo

input%vx(:,:)=wind*cos(alpha)
input%vy(:,:)=wind*sin(alpha)

! write variables to the file
call write_inputs(inputfile,input,grid)

! clean up and exit
deallocate(input%nfuel_cat,input%dzdxf,input%dzdyf,input%vx,input%vy,input%zsf)

end program model_test_init
