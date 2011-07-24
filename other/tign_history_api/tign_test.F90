! This program tests the lfn_history api.  It assumes a valid wrfinput_d01
! file exists in the current path.

program tign_test
use tign_history
implicit none

integer,parameter::idom=1
integer::nx,ny,i,mtime,sr_x,sr_y
real,allocatable,dimension(:,:)::tign
real::dx,dy,dt
real,parameter::testvalue=11.5

call get_grid_info(idom,nx=nx,ny=ny,sr_x=sr_x,sr_y=sr_y,dx=dx,dy=dy,dt=dt)
allocate(tign(nx,ny))

tign(:,:)=testvalue

call write_tign_history(idom,nx,ny,tign)

tign(:,:)=-1

call read_tign_history(idom,nx,ny,tign)

if(any(tign(:,:).ne.testvalue))then
  print*,'ERROR tign set incorrectly'
  call abort()
endif
print*,'TEST SUCCESSFUL'
end program tign_test
