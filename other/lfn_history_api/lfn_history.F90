!*** lfn_history ***
!
! A simple api for reading/writing level function histories in wrfinput files.
!
! The convention in this code is that two extra variables will be added to 
! the wrfinput_d## files.  
!
! 1. float LFN_HIST(nx,ny,ntime) contains the level function history for a 
!      total number of ntime time slices.  The array in the file will have 
!      3rd dimension of NHIST >= ntime, extra slices are to be ignored.
!
! 2. float LFN_TIME(ntime) contains the time of each lfn history array.  
!      This time is defined as the number of seconds after the start of 
!      the simulation.  These values should all be positive, a negative
!      value (in particular INVALID) will indicate the the level function,
!      and all time slices after it, does not contain valid data.
!
! The api takes care of all the particulars of reading/writing... it should
! be invisible to the caller.  There are 3 subroutines and one parameter
! contained in this api (see implementation for calling syntax):
!
! parameter:
!   NHIST, the maximum number of history slices the api can handle.
!
! subroutine:
!   write_lfn_history, write (or add) level function history data to
!                      the input file
!   read_lfn_history, read all level function history data from the 
!                     input file
!   get_grid_info, get any relevant information (such as grid size) from 
!                  input file

module lfn_history
use netcdf
implicit none
private

! Various parameters describing the layout of wrf files
integer,parameter::FILELEN=16,NHIST=15,XTYPE=nf90_float,INVALID=-9999999
character(len=FILELEN),parameter::FILEFMT="(A10,I02.2)"
character(len=FILELEN),parameter::FILEBSE='wrfinput_d'
character(len=nf90_max_name),parameter::   &
              XNAME='west_east_subgrid',   &
              YNAME='south_north_subgrid', &
              XATM='west_east_stag',       &
              YATM='south_north_stag',     &
              LFN_HIST_NAME='LFN_HIST',    &
              LFN_HIST_TIME='LFN_TIME',    &
              LFN_HIST_DIM='i_lfn_history',&
              DTNAME='DT',                 &
              DXNAME='DX',                 &
              DYNAME='DY'

public::NHIST,write_lfn_history,read_lfn_history,get_grid_info
              
contains


subroutine write_lfn_history(idom,nx,ny,ntime,lfn,time)
implicit none

! The subroutine writes a level function to a wrfinput file in the 
! current directory.  
!
! idom : Input integer describing the the domain number that we will
!        write the array to.  This is only to determine the file name
!        as printf 'wrfinput_d%02i' idom.
!
! nx,ny : Input size of level function (must be the same size as the fire
!         grid in the input file.  See get_grid_info to get this from the file.
!
! ntime : The number of time slices to write to the file.  This number must be 
!         less than or equal to NHIST.
!
! lfn(nx,ny,ntime) : The level function history from time(1) to time(ntime).
!
! time(ntime) : The time of each level function history slice relative to the 
!               simulation start time in seconds.  All times must be >= 0, with
!               The last time being the final history before the main fire 
!               code takes over computation. 

integer,intent(in)::idom,ntime,nx,ny
real,dimension(ntime),intent(in)::time
real,dimension(nx,ny,ntime),intent(in)::lfn
integer::lx,ly,vhist,vtime,ncid,i
real,dimension(NHIST)::xtime
if( ntime .gt. NHIST .or. ntime .lt. 1 )then
  print*,'invalid itime ',ntime
  call abort()
endif
do i=1,ntime
  if( time(i) .lt. 0 )then
    print*,'invalid time ',time(i)
    print*,'time must be >= 0 seconds relative to simulation start'
    call abort()
  endif
  if( i .gt. 1 .and. time(i) .le. time(max(1,i-1)) )then
    print*,'time slices must be sorted in order from earliest to latest.'
    call abort()
  endif
enddo
call get_grid_info(idom,nx=lx,ny=ly)
if(lx.ne.nx.or.ly.ne.ny)then
  print*,'invalid input level function size'
  call abort()
endif
xtime(:)=INVALID
xtime(1:ntime)=time(:)
call create_hist(idom,vhist,vtime)
ncid=open_file(idom,nf90_write)
call check(nf90_put_var(ncid,vhist,lfn,start=(/1,1,1/),count=(/nx,ny,ntime/)))
call check(nf90_put_var(ncid,vtime,(/xtime/),start=(/1/),count=(/NHIST/)))
call check(nf90_close(ncid))
end subroutine write_lfn_history

subroutine read_lfn_history(idom,nx,ny,ntime,lfn,time)
implicit none

! The subroutine reads a level function from a wrfinput file in the 
! current directory.  
!
! idom : Input integer describing the the domain number that we will
!        read the array from.  This is only to determine the file name
!        as printf 'wrfinput_d%02i' idom.
!
! nx,ny : Input size of level function (must be the same size as the fire
!         grid in the input file.  See get_grid_info to get this from the file.
!
! ntime : The number of time slices we will be reading from the file.  Gives
!         the bounds of the input arrays, lfn and time.
!         This must be the same as is returned by calling get_grid_info, 
!         with the optional argument ntime.
!
! lfn(nx,ny,ntime) : The level function history from time(1) to time(ntime),
!                    on return.
!
! time(ntime) : The time of each level function history slice relative to the 
!               simulation start time in seconds.

integer,intent(in)::idom,nx,ny,ntime
real,dimension(ntime),intent(out)::time
real,dimension(nx,ny,ntime),intent(out)::lfn
integer::lx,ly,vhist,vtime,ncid,ltime
real,dimension(1)::arr
real,dimension(NHIST)::htime
call get_grid_info(idom,ntime=ltime,nx=lx,ny=ly)
if( ltime .ne. ntime )then
  print*,'invalid ntime ',ntime
  print*,'this must be set from the output of get_grid_info'
  call abort()
endif
if(lx.ne.nx.or.ly.ne.ny)then
  print*,'invalid input level function size'
  call abort()
endif
call create_hist(idom,vhist,vtime)
ncid=open_file(idom,nf90_nowrite)
call read_time(ncid,time=htime)
call check(nf90_get_var(ncid,vhist,lfn,start=(/1,1,1/),count=(/nx,ny,ntime/)))
time(:)=htime(1:ntime)
call check(nf90_close(ncid))
end subroutine read_lfn_history

subroutine get_grid_info(idom,nx,ny,ntime,dx,dy,dt,sr_x,sr_y)
implicit none

! This subroutine inquires a wrfinput file in the current directory about
! information relevant to the computation and manipulation of lfn history
! arrays.  The return values are all optional so they can be used all at
! once or individually.  Calls to this subroutine should be made with the
! key=value syntax as more arguments may be added in the future to account
! for additional information required.
!
! idom : Input integer describing the the domain number that we will
!        read the array from.  This is only to determine the file name
!        as printf 'wrfinput_d%02i' idom.
!
! Optional outputs:
!
! nx,ny : The dimensions of the fire grid in the given file.
!
! ntime : The number of valid history slices available in the input file.
!         This argument can ONLY be used on a wrfinput file that already
!         contains the lfn history arrays, otherwise it is a fatal error.
!         The main purpose of this is to get proper bounds for the arrays
!         before a call to read_lfn_history.
!
! dx,dy : The grid resolution of the fire grid in meters.
!
! dt : The atmospheric time step in seconds.
!
! sr_x,sr_y : The atmospheric/fire grid refinement factor.

integer,intent(in)::idom
integer,optional,intent(out)::nx,ny,ntime,sr_x,sr_y
real,optional,intent(out)::dx,dy,dt

integer::ncid,n,m,lrx,lry

ncid=open_file(idom,nf90_nowrite)

! get subgrid refinement needed for several of the computations.
call get_dim(ncid,XNAME,n)
call get_dim(ncid,XATM,m)
lrx=n/m
call get_dim(ncid,YNAME,n)
call get_dim(ncid,YATM,m)
lry=n/m

if(present(nx))then
  call get_dim(ncid,XNAME,n)
  call get_dim(ncid,XATM,m)
  nx=n-lrx  ! correct for extra memory in fire grid arrays
endif
if(present(ny))then
  call get_dim(ncid,YNAME,n)
  call get_dim(ncid,YATM,m)
  ny=n-lry  ! correct for extra memory in fire grid arrays
endif
if(present(dx))then
  dx=get_attr(ncid,DXNAME)
endif
if(present(dy))then
  dy=get_attr(ncid,DYNAME)
endif
if(present(dt))then
  dt=get_attr(ncid,DTNAME)
endif
if(present(ntime))then
  call read_time(ncid,ntime=ntime)
endif
if(present(sr_x))sr_x=lrx
if(present(sr_y))sr_y=lry
call check(nf90_close(ncid))
end subroutine get_grid_info


character(len=FILELEN) function get_file_name(idom)
integer,intent(in)::idom
write(get_file_name,FILEFMT) FILEBSE,idom
end function get_file_name

subroutine read_time(ncid,ntime,time)
implicit none
integer,intent(in)::ncid
integer,optional,intent(out)::ntime
real,dimension(NHIST),optional,intent(out)::time

real,dimension(NHIST)::ltime
integer::varid,i
i=nf90_inq_varid(ncid,LFN_HIST_TIME,varid)
if(i.ne.nf90_noerr)then
  print*,'this file does not contain a valid lfn_time variable'
  print*,'perhaps it has not been initialized yet?'
  call abort()
endif
call check(nf90_get_var(ncid,varid,ltime))
if(present(time))time(:)=ltime(:)
if(present(ntime))then
  ntime=NHIST
  do i=1,NHIST
    if(ltime(i).eq.INVALID)then
      ntime=i-1
    elseif(ltime(i).lt.0)then
      print*,'invalid time value in wrfinput file, ',ltime(i)
      call abort()
    endif
  enddo
endif
end subroutine read_time

integer function open_file(idom,rw)
implicit none
integer,intent(in)::idom,rw
call check(nf90_open(trim(get_file_name(idom)),rw,open_file))
end function open_file

subroutine get_dim(ncid,dname,dlen,dimid)
implicit none
integer,intent(in)::ncid
character(len=*),intent(in)::dname
integer,optional,intent(out)::dlen,dimid
integer::did
call check(nf90_inq_dimid(ncid,dname,did))
if(present(dimid))dimid=did
if(present(dlen))then
  call check(nf90_inquire_dimension(ncid,did,len=dlen))
endif
end subroutine get_dim

real function get_attr(ncid,aname) result(aval)
implicit none
integer,intent(in)::ncid
character(len=*),intent(in)::aname
call check(nf90_get_att(ncid,nf90_global,aname,aval))
end function get_attr

subroutine create_hist(idom,ihist,itime)
implicit none
integer,intent(in)::idom
integer,intent(out)::ihist,itime
integer::ncid,ix,iy,ih,err
ncid=open_file(idom,nf90_write)
call check(nf90_redef(ncid))
err=nf90_def_dim(ncid,LFN_HIST_DIM,NHIST,ih)
call get_dim(ncid,XNAME,dimid=ix)
call get_dim(ncid,YNAME,dimid=iy)
err=nf90_def_var(ncid,LFN_HIST_NAME,XTYPE,(/ix,iy,ih/),ihist)
err=nf90_def_var(ncid,LFN_HIST_TIME,XTYPE,(/ih/),itime)
call check(nf90_enddef(ncid))
call check(nf90_inq_varid(ncid,LFN_HIST_NAME,ihist))
call check(nf90_inq_varid(ncid,LFN_HIST_TIME,itime))
call check(nf90_close(ncid))
end subroutine create_hist

subroutine check(ncout)
implicit none
integer,intent(in)::ncout
if(ncout.ne.nf90_noerr)then
  print*,'ERROR in netcdf call, ierr=',ncout
  print*,nf90_strerror(ncout)
  call abort()
endif
end subroutine check

end module lfn_history

#ifdef TESTING
program lfn_history_main
use lfn_history
implicit none
integer,parameter::i=1
integer::nx,ny,ncid,ntime,sr_x,sr_y
real::dx,dy,dt
call get_grid_info(i,nx=nx,ny=ny,dx=dx,dy=dy,dt=dt,sr_x=sr_x,sr_y=sr_y)
print*,nx,ny,dx,dy,dt,sr_x,sr_y
end program lfn_history_main
#endif
