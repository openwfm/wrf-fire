!*** A simple output module for wrf-fire standalone that produces output  READING 
!    files and reads input files that are somewhat compatible with wrf files
!

module wrf_netcdf
use netcdf
use module_fr_sfire_util , only : crash, interpolate_2d, continue_at_boundary
use module_domain, only: domain
implicit none

! default file names
character(len=*),parameter::inputfile='fire_input.nc'
character(len=*),parameter::outputfile='fire_output.nc'

! control whether the dimensions of the fire grid variables should have 
! the same (incorrect) sizes that wrf outputs, for compatibility
logical::compat_fire_grid=.true.

! output variable type
integer,parameter::vartype=nf90_float,field_type=104

! dimension names
character(len=nf90_max_name),parameter::                        &
                              dim_fire_x='west_east_subgrid',   &
                              dim_fire_y='south_north_subgrid', &
                              dim_atm_x='west_east',            &
                              dim_atm_y='south_north',          &
                              dim_atm_z='bottom_top',           &
                              dim_atm_x_s='west_east_stag',     &
                              dim_atm_y_s='south_north_stag',   &
                              dim_atm_z_s='bottom_top_stag',    &
                              dim_time='Time'
! variable names
character(len=nf90_max_name),parameter::                        &
                              var_lfn='LFN',                    &
                              unit_lfn='',                      &
                              desc_lfn='',                      &
                              var_tign_g='TIGN_G',                &
                              unit_tign_g='',                     &
                              desc_tign_g='',                     &
                              var_uf='UF',                      &
                              unit_uf='',                       &
                              desc_uf='',                       &
                              var_vf='VF',                      &
                              unit_vf='',                       &
                              desc_vf='',                       &
                              var_fuel_frac='FUEL_FRAC',        &
                              unit_fuel_frac='',                &
                              desc_fuel_frac='',                &
                              var_fire_area='FIRE_AREA',        &
                              unit_fire_area='',                &
                              desc_fire_area='',                &
                              var_fgrnhfx='FGRNHFX',            &
                              unit_fgrnhfx='',                  &
                              desc_fgrnhfx='',                  &
                              var_fgrnqfx='FGRNQFX',            &
                              unit_fgrnqfx='J/m^2',             &
                              desc_fgrnqfx='heat flux',         &
                              var_ros='ROS',                    &
                              unit_ros='m/s',                   &
                              desc_ros='ROS',                   &
                              var_r_0='R_0',                    &
                              unit_r_0='m/s',                   &
                              desc_r_0='R_0',                   &
                              var_flineint='FLINEINT',          &
                              unit_flineint='J/m/s',            &
                              desc_flineint='Byram fireline intensity',         &
                              var_flineint2='FLINEINT2',        & 
                              unit_flineint2='J/m/s^2',         & 
                              desc_flineint2='New fireline intensity'        
! inputs
character(len=nf90_max_name),parameter::                        &
                              var_nfuel_cat='NFUEL_CAT',        &
                              var_dzdxf='DZDXF',                &
                              var_dzdyf='DZDYF',                &
                              var_zsf='ZSF'                    
character(len=nf90_max_name),parameter::var_times='Times' 

! grid information structure
integer, parameter::times_length=19
type grid_info
  integer::nfirex,nfirey,      & ! fire grid size
           ntimes,             & ! number of times
           sr_x,sr_y,          & ! refinement factors, for WRF compatibility
           dimids(3)             ! dimension ids, to check
  real::fdx,fdy,dt               ! mesh and time resolution
  character(len=NF90_MAX_NAME)::dim_fire_x,dim_fire_y,dim_time ! dimension names, for info only
  character(len=times_length)::times ! the time string from the file
end type

! pointers to all variables being written (to simplify interface)
type output_vars_fire
  real,dimension(:,:),pointer::lfn,tign,fgrnhfx,fgrnqfx,fuel_frac
end type

! pointers to all variables being read (to simplify interface)
type input_vars_fire
  real,dimension(:,:),pointer::nfuel_cat,dzdxf,dzdyf,zsf,uf,vf,lfn,tign,fuel_frac
end type
type input_vars_atm
  real,dimension(:,:,:),pointer::u,v,ph,phb,z0
end type

contains

subroutine create_dim_attr(filename,info)

! Create an empty NetCDF file with proper dimension names 
! (a.k.a. attributes) defined.

implicit none

!*** arguments
character(len=*),intent(in)::filename  ! filename to create
type(grid_info),intent(in)::info       ! grid information structure

!*** local
integer::ncid,dimid,timeid,strlenid,varid

!*** executable

! create an empty file
call check(nf90_create(filename,nf90_clobber,ncid))

! define all dimensions

call create_dim(ncid,info%dim_time,nf90_unlimited)
call create_dim(ncid,info%dim_fire_x,info%nfirex)
call create_dim(ncid,info%dim_fire_y,info%nfirey)
call create_dim(ncid,'DateStrLen',times_length)

! define global attributes
call check(nf90_put_att(ncid,nf90_global,'FDX',info%fdx))
call check(nf90_put_att(ncid,nf90_global,'FDY',info%fdy))
call check(nf90_put_att(ncid,nf90_global,'DT',info%dt))
call check(nf90_put_att(ncid,nf90_global,'STANDALONE_DRIVER',1))

call check(nf90_enddef(ncid))
call check(nf90_close(ncid))

end subroutine create_dim_attr

subroutine create_dim(ncid,dimname,dimsize)
integer, intent(in):: ncid
character(len=*),intent(in)::dimname  ! dimension name to create
integer, intent(in):: dimsize
integer::dimid

!!The function NF90_DEF_DIM adds a new dimension to an open netCDF dataset in 
!!define mode. It returns (as an argument) a dimension ID, given the netCDF ID,
!!the dimension name, and the dimension length. (From NetCDF docs)

write(*,'(3a,i10)')'creating dimension ',trim(dimname),' size',dimsize
call check(nf90_def_dim(ncid,dimname,dimsize,dimid))
end subroutine create_dim

!
!***
!

subroutine create_output_file(filename,info)

!*** Create a file containing meta-data suitable for output of this simulation.
!    Only creates dimensions, variables, and attributes... does not populate
!    data.  Squash file if it already exists.

implicit none

!*** arguments
character(len=*),intent(in)::filename  ! filename to create
type(grid_info),intent(in)::info       ! grid information structure

!*** local
integer::ncid,dimid,timeid,strlenid,varid
character(len=NF90_MAX_NAME), dimension(3)::dim_fire

!*** executable

call create_dim_attr(filename,info)

call ncopen(filename,nf90_write,ncid)
call check(nf90_inq_dimid(ncid,'DateStrLen',strlenid))
call check(nf90_inq_dimid(ncid,dim_time,timeid))

! define a timekeeping variable
call check(nf90_redef(ncid))
call check(nf90_def_var(ncid,'Times',nf90_char,(/strlenid,timeid/),varid))
call check(nf90_enddef(ncid))
call check(nf90_close(ncid))

! create all of the output variables
dim_fire=(/info%dim_fire_x,info%dim_fire_y,info%dim_time/)
call define_var(filename,var_lfn,dim_fire,unit_lfn,desc_lfn)
call define_var(filename,var_tign_g,dim_fire,unit_tign_g,desc_tign_g)
call define_var(filename,var_fgrnhfx,dim_fire,unit_fgrnhfx,desc_fgrnhfx)
call define_var(filename,var_fgrnqfx,dim_fire,unit_fgrnqfx,desc_fgrnqfx)
call define_var(filename,var_fuel_frac,dim_fire,unit_fuel_frac,desc_fuel_frac)
call define_var(filename,var_fire_area,dim_fire,unit_fire_area,desc_fire_area)
call define_var(filename,var_flineint,dim_fire,unit_flineint,desc_flineint)
call define_var(filename,var_flineint2,dim_fire,unit_flineint2,desc_flineint2)
call define_var(filename,var_ros,dim_fire,unit_ros,desc_ros)
call define_var(filename,var_r_0,dim_fire,unit_r_0,desc_r_0)

end subroutine create_output_file

!
!***
!

subroutine define_var(filename,varname,dims,units,description)

!*** define a variable in a netcdf data set, the file is assumed to exist and
!    have valid meta-data (as created by create_output_file)

implicit none

!*** arguments
character(len=*),intent(in)::filename,varname  ! create variable varname in filename
character(len=*),dimension(:),intent(in)::dims ! the dimension names of the variable
character(len=*),intent(in)::units,description ! attributes created by wrf (not used at the moment)

!*** local
integer::ncid,i,varid
integer,dimension(4)::dimids
character(len=*),parameter::memorder='XYZ'
character(len=3)::stag

!*** executable
! open the file
call ncopen(filename,nf90_write,ncid)

! get dimension id's
do i=1,size(dims)
  call check(nf90_inq_dimid(ncid,dims(i),dimids(i)))
enddo

! enter define mode and define the variable
call check(nf90_redef(ncid))
call check(nf90_def_var(ncid,varname,vartype,dimids(1:size(dims)),varid))

! add attributes
call check(nf90_put_att(ncid,varid,'FieldType',field_type))
call check(nf90_put_att(ncid,varid,'MemoryOrder',memorder(1:size(dims))))
call check(nf90_put_att(ncid,varid,'description',description))
call check(nf90_put_att(ncid,varid,'units',units))
if(size(dims).eq.3)then
  stag='Z'
elseif(trim(dims(1)).eq.dim_atm_x_s)then
  stag='X'
elseif(trim(dims(2)).eq.dim_atm_y_s)then
  stag='Y'
elseif(trim(dims(3)).eq.dim_atm_z_s)then
  stag='Z'
else
  stag=''
endif
call check(nf90_put_att(ncid,varid,'stagger',stag))
call check(nf90_put_att(ncid,varid,'coordinates','XLONG XLAT'))

! close file
call check(nf90_enddef(ncid))
call check(nf90_close(ncid))
call print_var_dims(filename,varname)  ! check
end subroutine define_var

!
!***
!

subroutine write_vars(filename,grid,info,iframe)
!*** append variables to an output file (extending by the unlimited time
!    dimension)
implicit none

!*** arguments
character(len=*),intent(in)::filename
type(domain),intent(inout)::grid      ! the mother of all arrays
type(grid_info),intent(in)::info    ! dimensions
integer, intent(in)::iframe         ! frame to write into

!*** local
integer::ncid,dimid,varid

!*** executable

! open the file
call print_var_dims(filename,var_lfn)
call ncopen(filename,nf90_write,ncid)

write(*,'(3a,i4,2a)')'writing file ',trim(filename),' frame ',iframe,' time ',info%times 
! write out the current simulation time
call check(nf90_inq_varid(ncid,var_times,varid),'cannot find '//trim(var_times))
call check(nf90_put_var(ncid,varid,info%times,start=(/1,iframe/),count=(/times_length,1/)), &
     'error writing '//trim(var_times))

call write_fire_var(ncid,info,iframe,var_lfn,grid%lfn)
call write_fire_var(ncid,info,iframe,var_tign_g,grid%tign_g)
call write_fire_var(ncid,info,iframe,var_fuel_frac,grid%fuel_frac)
call write_fire_var(ncid,info,iframe,var_fire_area,grid%fire_area)
call write_fire_var(ncid,info,iframe,var_fgrnhfx,grid%fgrnhfx)
call write_fire_var(ncid,info,iframe,var_fgrnqfx,grid%fgrnqfx)
call write_fire_var(ncid,info,iframe,var_ros,grid%ros)
call write_fire_var(ncid,info,iframe,var_r_0,grid%r_0)
call write_fire_var(ncid,info,iframe,var_flineint,grid%flineint)
call write_fire_var(ncid,info,iframe,var_flineint2,grid%flineint2)

! close file
call latch
call check(nf90_close(ncid))
end subroutine write_vars

!
!*** READING 
!

subroutine read_info(filename,info)
! get fire grid sizes from input file
implicit none

!*** arguments
character(len=*),intent(in)::filename
type(grid_info),intent(inout)::info

!*** local
integer::ncid,it,ndims
integer, parameter:: mdims=3
integer,dimension(mdims)::dims,sr,dimids
real::dx,dy
character(len=128)::msg
character(len=NF90_MAX_NAME)::dimnames(mdims)

!*** executable
write(*,'(2a)')'reading dimensions from file ',trim(filename)
call ncopen(filename,nf90_nowrite,ncid)
call read_var_dims(ncid,var_nfuel_cat,mdims,ndims,dims,sr,dimnames,dimids)
if(ndims.ne.3)then
    write(msg,'(2a,i2,a,i2)')var_nfuel_cat,' has ',ndims,' dimensions not ',mdims
    call crash(msg)
endif
if((sr(1).ne.0.and.sr(2).eq.0).or.sr(1).lt.0.or.sr(2).lt.0)then
    write(msg,'(a,2i5)')'bad subgrid refinement ratios',sr(1),sr(2)
    call crash(msg)
endif
info%nfirex=dims(1)
info%nfirey=dims(2)
info%ntimes=dims(3)
info%dim_fire_x=dimnames(1)
info%dim_fire_y=dimnames(2)
info%dim_time=dimnames(3)
info%dimids=dimids
info%sr_x=sr(1)
info%sr_y=sr(2)
	
call att_read(ncid,'DX',info%fdx)
call att_read(ncid,'DY',info%fdy)
if(sr(1).ne.0)then
    info%fdx=info%fdx/sr(1)
    info%fdy=info%fdy/sr(2)
endif
call att_read(ncid,'DT',info%dt)

write(*,'(4(a,1x))')'dimension names:',trim(info%dim_fire_x),trim(info%dim_fire_y),trim(info%dim_time)
write(*,'(a,2i6)')'fire grid dimensions:',info%nfirex,info%nfirey
write(*,'(a,i6)')'number of time frames',info%ntimes
write(*,'(3(a,f8.4,1x))')'stepsizes fdx=',info%fdx,'fdy=',info%fdy,'dt=',info%dt
call check(nf90_close(ncid))
end subroutine read_info

subroutine dim_read(ncid,dim_name,dim_len)
character(len=*),intent(in)::dim_name
integer, intent(in)::ncid
integer,intent(out):: dim_len
integer:: dim_id
call check(nf90_inq_dimid(ncid,dim_name,dim_id))
call check(nf90_inquire_dimension(ncid,dim_id,len=dim_len))
write(*,'(a,1x,a,i6)')'dimension',trim(dim_name),dim_len
end subroutine dim_read

subroutine att_read(ncid,att_name,att_val)
integer, intent(in)::ncid
character(len=*),intent(in)::att_name
real, intent(out)::att_val
call check(nf90_get_att(ncid,nf90_global,att_name,att_val))
write(*,'(a,1x,a,g20.5)')'attribute',trim(att_name),att_val
end subroutine att_read


!
!***
!

subroutine print_var_dims(filename,varname)
character(len=*), intent(in)::filename,varname
integer, parameter:: mdims=3
integer::ncid,ndims,dims(mdims),dimids(mdims),i
character(len=NF90_MAX_NAME)::dimnames(mdims)
call ncopen(filename,nf90_nowrite,ncid)
write(*,'(3a)')'trying to read dimensions of variable ',varname
call read_var_dims(ncid,varname,mdims,ndims,dims,dimnames=dimnames,no_check_stag=.true.)
write(*,'(3a,4(2a,i5,1x))')'variable ',varname,' dimensions ',(dimnames(i),'=',dims(i),i=1,ndims)
call check(nf90_close(ncid))
end subroutine print_var_dims

subroutine read_var_dims(ncid,varname,mdims,ndims,dims,sr,dimnames,dimids,no_check_stag)
! get variable dimensions from a file
implicit none
! arguments
integer, intent(in):: ncid             ! open netcdf file
character(len=*), intent(in):: varname ! variable name
integer, intent(in):: mdims            ! maximal number of dimensions
integer, intent(out)::ndims            ! number of dimensions of this variable
integer, intent(out)::dims(mdims)      ! the dimensions
integer, intent(out), optional::sr(mdims)! refinement ratios
character(len=*),intent(out),optional::dimnames(mdims) ! dimension names
integer,intent(out),optional::dimids(mdims) ! dimension ids
logical, intent(in), optional:: no_check_stag
! local
integer::varid,dimid,xtype,idimids(mdims),idim,i,ratio,staglen,stagid,dimlen
character(len=NF90_MAX_NAME)::dimname,stagname
character(len=128)::msg
character(len=*), parameter:: subgrid='_subgrid'
! executable
write(*,'(2a)')'reading variable ',trim(varname)
call check(nf90_inq_varid(ncid, varname, varid))
call check(nf90_inquire_variable(ncid,varid,xtype=xtype,ndims=ndims))
if(ndims.gt.mdims)then
    write(*,1)'variable ',trim(varname),' has ',ndims,' dimensions >',mdims
    call crash('variable has too many dimensions')
1 format(3a,i5,a,(4i6))
endif
if(xtype.ne.NF90_FLOAT)then
    call crash('only FLOAT variable allowed')
endif
call check(nf90_inquire_variable(ncid, varid, dimids=idimids))
do idim=1,ndims
    write(*,'(a,i3,a,i6)')'inquiring dimension ',idim,' id',idimids(idim)
    call check(nf90_inquire_dimension(ncid, idimids(idim), dimname, dimlen))
    write(*,'(3a,i5)')'got dimension name ',trim(dimname),' length',dimlen
    if(.not.present(no_check_stag))then
    ratio=0
    i=index(dimname,subgrid,.true.)
    ! print *,trim(dimname),subgrid,i,len(subgrid),len_trim(dimname)
    if ( i + len(subgrid) .eq. len_trim(dimname)+1) then ! subgrid, need to fix
        stagname=dimname(1:i-1) // '_stag'
        call check(nf90_inq_dimid(ncid, stagname, stagid))
        call check(nf90_inquire_dimension(ncid, stagid, len=staglen))
        ratio = dimlen/staglen
    endif
    dims(idim) = dimlen - ratio 
    if(present(sr))then
        sr(idim)=ratio
    endif
    endif
    if(present(dimnames))then
        dimnames(idim)=dimname
    endif
    if(present(dimids))then
        dimids(idim)=idimids(idim)
    endif
enddo
2 format(3a,(4i6))
write(*,2)'variable ',trim(varname),    ' dimensions    ',(dims(idim),idim=1,ndims)
if(present(sr))then
    write(*,2)'variable ',trim(varname),' subgrid ratios',(sr(idim),idim=1,ndims)
endif
end subroutine read_var_dims

!
!***
!

subroutine write_fire_var(ncid,info,iframe,varname,v)
implicit none
!*** arguments
integer, intent(in)::ncid             ! id of netcdf file open in data mode
type(grid_info),intent(in)::info      ! dimensions
integer, intent(in):: iframe          ! number of frame in the file
character(len=*),intent(in)::varname  ! the variable name
real, intent(in):: v(:,:)            ! values
!*** local
integer::varid,nx,ny,it,start(3),count(3)
!*** executable
nx=info%nfirex
ny=info%nfirey

write(*,'(3a,2i5)')'writing variable ',trim(varname),' size ',nx,ny
call check(nf90_inq_varid(ncid,varname,varid),'cannot find '//trim(varname))
call check(nf90_put_var(ncid,varid,v(1:nx,1:ny),start=(/1,1,iframe/),count=(/nx,ny,1/)), &
     'error writing '//trim(varname))

end subroutine write_fire_var

!
!***
!

subroutine read_fire_var(ncid,info,iframe,varname,v)
implicit none
!*** arguments
integer, intent(in)::ncid             ! id of netcdf file open in data mode
type(grid_info),intent(in)::info      ! dimensions
integer, intent(in):: iframe          ! number of frame in the file
character(len=*),intent(in)::varname  ! the variable name
real, intent(out):: v(:,:)            ! values
!*** local
integer::varid,nx,ny,it,start(3),count(3)
!*** executable
nx=info%nfirex
ny=info%nfirey

write(*,'(2a)')'reading variable ',trim(varname)
call check(nf90_inq_varid(ncid,varname,varid),'cannot find '//trim(varname))
call check(nf90_get_var(ncid,varid,v(1:nx,1:ny),start=(/1,1,iframe/),count=(/nx,ny,1/)), &
     'error reading '//trim(varname))

end subroutine read_fire_var

!
!***
!

subroutine read_vars(filename,info,iframe,grid)
! read all variables from input file
implicit none

!*** arguments
character(len=*),intent(in)::filename ! the input file
type(grid_info),intent(inout)::info   ! dimensions
integer, intent(in):: iframe          ! number of frame in the file
type(domain),intent(inout)::grid      ! the mother of all arrays

!*** local
integer::ncid,varid,ierr
integer,dimension(4)::s,c

!*** executable
write(*,'(3a,i4)')'reading file ',trim(filename),' frame ',iframe 
call ncopen(filename,nf90_nowrite,ncid)

call check(nf90_inq_varid(ncid,var_times,varid),'cannot find '//trim(var_times))
call check(nf90_get_var(ncid,varid,info%times,start=(/1,iframe/),count=(/times_length,1/)), &
     'error reading '//trim(var_times))
write(*,'(2a)')'Time ',info%times

call read_fire_var(ncid,info,iframe,var_nfuel_cat,grid%nfuel_cat) 
call read_fire_var(ncid,info,iframe,var_dzdxf,grid%dzdxf) 
call read_fire_var(ncid,info,iframe,var_dzdyf,grid%dzdyf) 
call read_fire_var(ncid,info,iframe,var_uf,grid%uf) 
call read_fire_var(ncid,info,iframe,var_vf,grid%vf) 

call check(nf90_close(ncid))
end subroutine read_vars

!
!***
!

subroutine check(ncerr,msg,cont)
implicit none
integer,intent(in)::ncerr
character(len=*), optional, intent(in)::msg
logical, intent(in), optional::cont
character(len=128)::message
if(ncerr.ne.nf90_noerr)then
  write(6,'(2a)')"NetCDF error: ",trim(nf90_strerror(ncerr))
  if(present(msg))then
    message=msg
  else
    message="NETCDF ERROR"
  endif
  if(present(cont))then
    if(cont)return
  endif
  call crash(message)
endif
end subroutine check

!
!***
!

subroutine ncopen(filename,mode,ncid)
!*** purpose: open netcdf file with an informative error message 
implicit none
!*** arguments
character(len=*), intent(in):: filename
integer, intent(in)::mode
integer, intent(out):: ncid
!*** executable
call check(nf90_open(filename,mode,ncid),"Cannot open file "//trim(filename))
end subroutine ncopen

end module wrf_netcdf
