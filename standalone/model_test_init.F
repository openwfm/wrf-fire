! A simple program to initialize a test case for wrf-fire standalone driver.  
! It will create a netcdf file called fire_input.nc containing initial
! conditions for the fire simulation.

program model_test_init
use wrf_namelist
use wrf_netcdf
use module_fr_sfire_util, only : crash
implicit none

type(grid_info)::grid
type(input_vars)::input
integer::ierr,i,j
real,parameter::wind=10.,alpha=0.0

! read namelist parameters
call read_namelist()

! get grid information
grid%natmx=e_we(1)-s_we(1)+1
grid%natmy=e_sn(1)-s_sn(1)+1
grid%natmz=e_vert(1)-s_vert(1)+1
grid%sr_x=sr_x(1)
grid%sr_y=sr_y(1)
grid%nfirex=grid%natmx*sr_x(1)
grid%nfirey=grid%natmy*sr_y(1)
grid%dx=dx(1)
grid%dy=dy(1)
grid%dt=real(time_step)+real(time_step_fract_num)/real(time_step_fract_den)

! allocate space for input variables
allocate(input%nfuel_cat(grid%nfirex,grid%nfirey),input%dzdxf(grid%nfirex,grid%nfirey), &
         input%dzdyf(grid%nfirex,grid%nfirey),input%vx(grid%natmx+1,grid%natmy), &
         input%vy(grid%natmx,grid%natmy+1),input%zsf(grid%nfirex,grid%nfirey))

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
deallocate(input%nfuel_cat,input%dzdxf,input%dzdyf,input%vx,input%vy)
end program model_test_init
