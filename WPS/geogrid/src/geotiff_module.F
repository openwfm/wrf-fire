#ifdef _TESTING_GEOTIFF
#define _HAS_GEOTIFF 1
#endif

#ifdef _HAS_GEOTIFF

module geotiff_module
use module_debug
use misc_definitions_module
implicit none

type proj_t
  private
  ! geotiff projection type
  integer::proj_id
  real::dx,dy,stdlon,truelat1,truelat2
  integer::known_x,known_y
  real::known_lat,known_lon
end type proj_t

type geotiff_t
  private
  ! contains all needed metadata from an open geotiff file, including
  ! the file handle
  
  ! geotiff file handle stored as an array of characters, because the 
  ! length of the handle can vary, we will query the tiff library
  ! and allocate the necessary memory before opening.
  !character(len=1),dimension(:),pointer::gtiff_p
  integer::gtiff_p

  ! global size of the image
  integer :: nx,ny,nz

  ! tile size of the image
  integer :: tx,ty

  ! tile border size
  integer :: bdr

  ! source missing value
  real :: missing

  ! categorical flag
  logical :: categorical

  ! projection information
  type(proj_t) :: proj
end type geotiff_t

integer,parameter::r_invalid=-1,i_invalid=-1
integer,dimension(5),parameter::geotiff_projections= &
      (/PROJ_LC,PROJ_PS,PROJ_MERC,PROJ_LATLON,PROJ_ALBERS_NAD83/)
integer,parameter::MAX_GEOTIFF_FILES=32, MAX_FILE_LENGTH=MAX_FILENAME_LEN
character(len=MAX_FILE_LENGTH),dimension(MAX_GEOTIFF_FILES),save::geotiff_file_names,geotiff_field_names
type(geotiff_t),dimension(MAX_GEOTIFF_FILES),target,save::geotiff_file_p
integer,save::nfiles=0

interface check_invalid
  module procedure check_invalid_i,check_invalid_r
end interface

contains

subroutine open_geotiff(filename,fieldname)
implicit none

!*** Open a geotiff file and save the handle in geotiff_file_p.
character(len=*),intent(in)::filename ! geotiff file to open
character(len=*),intent(in)::fieldname
type(geotiff_t),pointer::g
integer::nz,status
character(len=MAX_FILE_LENGTH)::cfile

if(nfiles+1 > MAX_GEOTIFF_FILES)then
  call mprintf(.true.,ERROR,"Number of open geotiff files exceeds MAX_GEOTIFF_FILES.")
endif

nfiles=nfiles+1

cfile=ADJUSTL(filename)
cfile(LEN_TRIM(filename)+1:LEN_TRIM(filename)+1)=char(0)

g=>geotiff_file_p(nfiles)
geotiff_file_names(nfiles)=filename
geotiff_field_names(nfiles)=fieldname
call open_geotiff_l(cfile,g%gtiff_p)
call geotiff_header(g%gtiff_p,g%nx,g%ny,g%nz,g%tx,g%ty,g%proj%proj_id,g%proj%dx, &
                    g%proj%dy,g%proj%known_x,g%proj%known_y,g%proj%known_lat,    &
                    g%proj%known_lon,g%proj%stdlon,       &
                    g%proj%truelat1,g%proj%truelat2,status)
if(status .ne. 0)then
  call mprintf(.true.,ERROR,"Error reading geotiff file "//TRIM(filename))
endif

! add defaults
g%missing=-99
g%tx=256
g%ty=256
g%bdr=3
g%categorical=.false.
g%nz=1
end subroutine open_geotiff

subroutine merge_geotiff_header(field,is_proj,proj,is_fieldtype,fieldtype,     &
                                is_units,units,is_description,description,     &
                                is_dx,dx,is_dy,dy,                             &
                                is_known_x,known_x,is_known_y,known_y,         &
                                is_known_lat,known_lat,is_known_lon,known_lon, &
                                is_stdlon,stdlon,is_truelat1,truelat1,         &
                                is_truelat2,truelat2,is_row_order,row_order,   &
                                is_tile_x,tile_x,is_tile_y,tile_y,             &
                                is_tile_z,tile_z,is_tile_z_start,tile_z_start, &
                                is_tile_z_end,tile_z_end,is_category_min,      &
                                category_min,is_category_max,category_max,     &
                                is_tile_bdr,tile_bdr,is_missing_value,         &
                                missing_value)
implicit none

character(len=*),intent(in)::field
logical,intent(inout)::is_proj,is_fieldtype,is_units,is_description,is_dx,is_dy,  &
                       is_known_x,is_known_y,is_known_lat,is_known_lon,is_stdlon, &
                       is_truelat1,is_truelat2,is_row_order,is_tile_x,is_tile_y,  &
                       is_tile_z,is_tile_z_start,is_tile_z_end,is_category_min,   &
                       is_category_max,is_tile_bdr,is_missing_value
integer,intent(inout)::proj,fieldtype,row_order,tile_x,tile_y,tile_z,          &
                       tile_z_start,tile_z_end,category_min,category_max,      &
                       tile_bdr
real,intent(inout)::dx,dy,known_x,known_y,known_lat,known_lon,stdlon,truelat1, &
                    truelat2,missing_value
character(len=*),intent(inout)::units,description

type(geotiff_t),pointer::g

call get_geotiff_def(field,g)

if(.not.is_proj)then
  call check_invalid("projection",g%proj%proj_id)
  proj=geotiff_projections(g%proj%proj_id)
  is_proj=.true.
endif
if(.not.is_dx)then
  call check_invalid("dx",g%proj%dx)
  dx=g%proj%dx
  is_dx=.true.
endif
if(.not.is_dy)then
  call check_invalid("dy",g%proj%dy)
  dy=g%proj%dy
  is_dy=.true.
endif
if(.not.is_known_x)then
  call check_invalid("known_x",g%proj%known_x)
  known_x=g%proj%known_x
  is_known_x=.true.
endif
if(.not.is_known_y)then
  call check_invalid("known_y",g%proj%known_y)
  known_y=g%proj%known_y
  is_known_y=.true.
endif
if(.not.is_known_lat)then
  call check_invalid("known_lat",g%proj%known_lat)
  known_lat=g%proj%known_lat
  is_known_lat=.true.
endif
if(.not.is_known_lon)then
  call check_invalid("known_lon",g%proj%known_lon)
  known_lon=g%proj%known_lon
  is_known_lon=.true.
endif
if(.not.is_stdlon.and.proj.ne.PROJ_LATLON)then
  call check_invalid("stdlon",g%proj%stdlon)
  stdlon=g%proj%stdlon
  is_stdlon=.true.
endif
if(.not.is_truelat1.and.proj.ne.PROJ_LATLON)then
  call check_invalid("truelat1",g%proj%truelat1)
  truelat1=g%proj%truelat1
  is_truelat1=.true.
endif
if(.not.is_truelat2.and.proj.ne.PROJ_LATLON)then
  call check_invalid("truelat2",g%proj%truelat2)
  truelat2=g%proj%truelat2
  is_truelat2=.true.
endif
if(is_tile_x)then
  g%tx=tile_x
endif
if(is_tile_y)then
  g%ty=tile_y
endif
if(is_fieldtype)then
  g%categorical=(fieldtype.eq.CATEGORICAL)
endif
if(is_tile_z)then
  g%nz=tile_z
endif
if(is_tile_z_start.and.is_tile_z_end)then
  g%nz=tile_z_end-tile_z_start+1
endif
if(.not.(is_category_min.and.is_category_max).and.g%categorical)then
  call mprintf(.true.,ERROR,"Field %s marked as categorical with no category bounds", &
          s1=TRIM(field))
endif
if(is_tile_bdr)then
  g%bdr=tile_bdr
endif
if(is_missing_value)then
  g%missing=missing_value
endif
end subroutine merge_geotiff_header

subroutine check_invalid_i(parm,val)
implicit none
character(len=*),intent(in)::parm
integer,intent(in)::val
call check_invalid_l(parm,ival=val)
end subroutine check_invalid_i

subroutine check_invalid_r(parm,val)
implicit none
character(len=*),intent(in)::parm
real,intent(in)::val
call check_invalid_l(parm,rval=val)
end subroutine check_invalid_r

subroutine check_invalid_l(parm,ival,rval)
implicit none
character(len=*),intent(in)::parm
integer,optional,intent(in)::ival
real,optional,intent(in)::rval
if(present(ival))then
  if(ival .eq. I_INVALID) goto 99
endif
if(present(rval))then
  if(rval .eq. R_INVALID) goto 99
endif
return
 99 continue
call mprintf(.true.,ERROR,"Parameter %s, not set in index or geotiff file.",s1=TRIM(parm))
end subroutine check_invalid_l

subroutine open_geotiff_l(filename,gtiff_p)
implicit none

!*** Private stub to geotiff library.  Allocate a pointer for storage,
!    and store the TIFF file handle.

character(len=*),intent(in)::filename
integer,intent(out)::gtiff_p

integer::ptr_size,i,stat

call geotiff_open(filename,gtiff_p,stat)!gtiff_p)

end subroutine open_geotiff_l

subroutine close_geotiff_l(gtiff_p)
implicit none

!*** close a tiff file and deallocate pointer memory

integer,intent(inout)::gtiff_p
call geotiff_close(gtiff_p)
gtiff_p=-1
end subroutine close_geotiff_l

subroutine destroy_all()
implicit none
!*** close all files and free allocated memory
integer::i
do i=1,nfiles
  geotiff_file_names(i)=''
  geotiff_field_names(i)=''
  call close_geotiff_l(geotiff_file_p(i)%gtiff_p)
  geotiff_file_p%nx=-1
  geotiff_file_p%ny=-1
  geotiff_file_p%tx=-1
  geotiff_file_p%ty=-1
end do
end subroutine destroy_all

subroutine index2tile(nt,ig,it,lt)
implicit none

!*** Convert global index to tile index (1D).
!    input:
!      integer, nt : tile dimension
!      integer, ig : global index
!    output:
!      integer, it : tile containing global index i
!      integer, lt : local index within tile

integer,intent(in)::nt,ig
integer,intent(out)::it,lt

it=(ig-1)/nt
lt=ig - it*nt + 1
it=it+1

end subroutine index2tile

subroutine index2tilebound(nt,is,ie,ts,ls,te,le)
implicit none

!*** For given global index bounds, get tile index bounds that cover the 
!    requested interval.
!    input:
!      integer, nt : tile dimension
!      integer, is, ie : global start and end indices 
!    output:
!      integer, ts,te : tile start and end indices
!      integer, ls,le : local start and end indices

integer,intent(in)::nt,is,ie
integer,intent(out)::ts,ls,te,le

call index2tile(nt,is,ts,ls)
call index2tile(nt,ie,te,le)

end subroutine index2tilebound

subroutine read_tile_l(gtiff,nx,ny,nz,ix,iy,tile,stat)
implicit none

!*** A stub to the geotiff library.  Tile dimensions given must be the same
!    as contained in the geotiff file.  If ix or iy are outside of the bounds
!    or any read error occurs, return stat > 0.
!    input:
!      geotiff_t, gtiff : geotiff file structure
!      integer, nx,ny  : tile dimensions
!      integer, ix,iy  : tile indices
!    output:
!      real, tile(nx,ny) : tile data read from file
!      integer, stat : error status
integer,intent(in)::gtiff
integer,intent(in)::nx,ny,nz,ix,iy
real,dimension(nx,ny),intent(out)::tile
integer,intent(out)::stat
call read_geotiff_tile(gtiff,ix,iy,nx,ny,nz,tile,stat)
end subroutine read_tile_l

subroutine read_geogrid_tile(field,xs,xe,ys,ye,zs,ze, &
                             array,stat)
implicit none
character(len=*),intent(in)::field
integer,intent(in)::xs,xe,ys,ye,zs,ze
real,dimension(xs:xe,ys:ye,zs:ze),intent(out)::array
integer,intent(out)::stat
type(geotiff_t),pointer::gtif
integer::xts,xte,xls,xle,yts,yte,yls,yle
real,dimension(:,:,:),allocatable::buffer
integer::i,j,xlsl,xlel,ylsl,ylel,is,ie,js,je

call get_geotiff_def(field,gtif)
call index2tilebound(gtif%tx,xs,xe,xts,xls,xte,xle)
call index2tilebound(gtif%ty,ys,ye,yts,yls,yte,yle)

allocate(buffer(gtif%tx,gtif%ty,gtif%nz))

is=xs

!print*,xs,xe,ys,ye

do i=xts,xte
  js=ys
  do j=yts,yte

    call read_tile_l(gtif%gtiff_p,gtif%tx,gtif%ty,gtif%nz,i,j,buffer,stat)
    xlsl=1
    ylsl=1
    xlel=gtif%tx
    ylel=gtif%ty
    if(i.eq.xts)xlsl=xls
    if(i.eq.xte)xlel=xle
    if(j.eq.yts)ylsl=yls
    if(j.eq.yte)ylel=yle
    
    ie=is+xlel-xlsl
    je=js+ylel-ylsl

!    print*,is,ie,js,je,buffer(xlsl,ylsl,1)
    if(stat .eq. 0)then
      array(is:ie,js:je,:)=buffer(xlsl:xlel,ylsl:ylel,:)
    else
      array(is:ie,js:je,:)=gtif%missing
    endif

    js=je+1

  enddo
  is=ie+1
enddo

end subroutine read_geogrid_tile

subroutine get_geotiff_def(field,gtif)
implicit none
character(len=*),intent(in)::field
type(geotiff_t),pointer,intent(out)::gtif
integer::i
do i=1,nfiles
  if(    TRIM(geotiff_field_names(i)) .eq.     TRIM(field) .and. &
     LEN_TRIM(geotiff_field_names(i)) .eq. LEN_TRIM(field))then
    goto 10
  endif
enddo
call mprintf(.true.,ERROR,"No open geotiff file for field "//TRIM(field))
10 continue
gtif=>geotiff_file_p(i)
end subroutine get_geotiff_def

end module geotiff_module

#else
subroutine dummy_fortran_subroutine()
implicit none
end subroutine dummy_fortran_subroutine
#endif
