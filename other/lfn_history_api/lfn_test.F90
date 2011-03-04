! This program tests the lfn_history api.  It assumes a valid wrfinput_d01
! file exists in the current path.

program lfn_test
use lfn_history
implicit none

integer,parameter::idom=1,ntime=4
integer::nx,ny,i,mtime,sr_x,sr_y
real,allocatable,dimension(:,:,:)::lfn
real,allocatable,dimension(:)::time
real::dx,dy,dt

call get_grid_info(idom,nx=nx,ny=ny,sr_x=sr_x,sr_y=sr_y,dx=dx,dy=dy,dt=dt)
allocate(lfn(nx,ny,ntime),time(ntime))

do i=1,ntime
  lfn(:,:,i)=i
  time(i)=i
enddo

call write_lfn_history(idom,nx,ny,ntime,lfn,time)

lfn(:,:,:)=-1
time(:)=-1

call get_grid_info(idom,ntime=mtime)
call read_lfn_history(idom,nx,ny,ntime,lfn,time)

if(ntime.ne.mtime)then
  print*,'ERROR ntime calculated incorrectly'
  call abort()
endif
do i=1,ntime
  if(any(lfn(:,:,i).ne.i))then
    print*,'ERROR lfn set incorrectly at time=',i
    call abort()
  endif
  if(time(i).ne.i)then
    print*,'ERROR time set incorrectly at i=',i
    call abort()
  endif
enddo
print*,'TEST SUCCESSFUL'
end program lfn_test
