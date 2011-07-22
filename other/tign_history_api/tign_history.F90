!*** tign_history ***
!
! A simple api for reading/writing level function histories in wrfinput files.
!
! The convention in this code is that two extra variables will be added to 
! the wrfinput_d## files.  
!
! 1. float tign_g(nx,ny) contains the fire history as the time in seconds from
!    the start of the simulation that each point ignited.
!
! The api takes care of all the particulars of reading/writing... it should
! be invisible to the caller.
!
! subroutine:
!   write_tign_history, write (or add) level function history data to
!                      the input file
!   read_tign_history, read all level function history data from the 
!                     input file
!   get_grid_info, get any relevant information (such as grid size) from 
!                  input file

module tign_history
use netcdf
implicit none
private

! Various parameters describing the layout of wrf files
integer,parameter::FILELEN=16,XTYPE=nf90_float,INVALID=-9999999,write_time=1
character(len=FILELEN),parameter::FILEFMT="(A10,I02.2)"
character(len=FILELEN),parameter::FILEBSE='wrfinput_d'
character(len=nf90_max_name),parameter::   &
              XNAME='west_east_subgrid',   &
              YNAME='south_north_subgrid', &
              TNAME='Time',                &
              XATM='west_east_stag',       &
              YATM='south_north_stag',     &
              tign_NAME='TIGN_G',          &
              DTNAME='DT',                 &
              DXNAME='DX',                 &
              DYNAME='DY'

public::write_tign_history,read_tign_history,get_grid_info
              
contains


subroutine write_tign_history(idom,nx,ny,tign)
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
! tign(nx,ny) : The level function history from time(1) to time(ntime).

integer,intent(in)::idom,nx,ny
real,dimension(nx,ny),intent(in)::tign
integer::lx,ly,vhist,ncid,i
call get_grid_info(idom,nx=lx,ny=ly)
if(lx.ne.nx.or.ly.ne.ny)then
  print*,'invalid input level function size'
  call abort()
endif
call create_hist(idom,vhist)
ncid=open_file(idom,nf90_write)
call check(nf90_put_var(ncid,vhist,tign,start=(/1,1,write_time/),count=(/nx,ny,1/)))
call check(nf90_close(ncid))
end subroutine write_tign_history

subroutine read_tign_history(idom,nx,ny,tign)
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
! tign(nx,ny) : The fire history read from the input file.

integer,intent(in)::idom,nx,ny
real,dimension(nx,ny),intent(out)::tign
integer::lx,ly,vhist,vtime,ncid,ltime
real,dimension(1)::arr
call get_grid_info(idom,nx=lx,ny=ly)
if(lx.ne.nx.or.ly.ne.ny)then
  print*,'invalid input array size'
  call abort()
endif
call create_hist(idom,vhist)
ncid=open_file(idom,nf90_nowrite)
call check(nf90_get_var(ncid,vhist,tign,start=(/1,1,write_time/),count=(/nx,ny,1/)))
call check(nf90_close(ncid))
end subroutine read_tign_history

subroutine get_grid_info(idom,nx,ny,dx,dy,dt,sr_x,sr_y)
implicit none

! This subroutine inquires a wrfinput file in the current directory about
! information relevant to the computation and manipulation of tign history
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
! dx,dy : The grid resolution of the fire grid in meters.
!
! dt : The atmospheric time step in seconds.
!
! sr_x,sr_y : The atmospheric/fire grid refinement factor.

integer,intent(in)::idom
integer,optional,intent(out)::nx,ny,sr_x,sr_y
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
if(present(sr_x))sr_x=lrx
if(present(sr_y))sr_y=lry
call check(nf90_close(ncid))
end subroutine get_grid_info

character(len=FILELEN) function get_file_name(idom)
integer,intent(in)::idom
write(get_file_name,FILEFMT) FILEBSE,idom
end function get_file_name

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

subroutine create_hist(idom,ihist)
implicit none
integer,intent(in)::idom
integer,intent(out)::ihist
integer::ncid,ix,iy,ih,err
ncid=open_file(idom,nf90_write)
call check(nf90_redef(ncid))
call get_dim(ncid,XNAME,dimid=ix)
call get_dim(ncid,YNAME,dimid=iy)
call get_dim(ncid,TNAME,dimid=ih)
err=nf90_def_var(ncid,tign_NAME,XTYPE,(/ix,iy,ih/),ihist)
call check(nf90_enddef(ncid))
call check(nf90_inq_varid(ncid,tign_NAME,ihist))
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

end module tign_history

#ifdef TESTING
program tign_history_main
use tign_history
implicit none
integer,parameter::i=1
integer::nx,ny,ncid,sr_x,sr_y
real::dx,dy,dt
call get_grid_info(i,nx=nx,ny=ny,dx=dx,dy=dy,dt=dt,sr_x=sr_x,sr_y=sr_y)
print*,nx,ny,dx,dy,dt,sr_x,sr_y
end program tign_history_main
#endif
