!*** A simple output module for wrf-fire standalone that produces output  READING 
!    files and reads input files that are somewhat compatible with wrf files
!

module wrf_netcdf
use netcdf
use module_fr_sfire_util , only : crash, interpolate_2d, continue_at_boundary
use module_domain, only: domain
implicit none

! default input file name
character(len=*),parameter::inputfile='fire_input.nc'

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
character(len=nf90_max_name),dimension(3),parameter::           &
        dim_fire=(/dim_fire_x,dim_fire_y,dim_time/)
character(len=nf90_max_name),dimension(4),parameter::           &
        dim_atm=(/dim_atm_x,dim_atm_y,dim_atm_z,dim_time/),     &
        dim_atm_u=(/dim_atm_x_s,dim_atm_y,dim_atm_z,dim_time/), &
        dim_atm_v=(/dim_atm_x,dim_atm_y_s,dim_atm_z,dim_time/), &
        dim_atm_w=(/dim_atm_x,dim_atm_y,dim_atm_z_s,dim_time/)

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
                              unit_fgrnqfx='',                  &
                              desc_fgrnqfx=''
character(len=nf90_max_name),parameter::                        &
                              var_nfuel_cat='NFUEL_CAT',        &
                              var_u='U',                        &
                              var_v='V',                        &
                              var_z0='Z0',                      &
                              var_ph='PH',                      &
                              var_phb='PHB',                    &
                              var_dzdxf='DZDXF',                &
                              var_dzdyf='DZDYF',                &
                              var_zsf='ZSF'

! grid information structure
type grid_info
  integer::nfirex,nfirey,      & ! fire grid size
           ntimes,             & ! number of times
           dimids(3)             ! dimension ids, to check
  real::fdx,fdy,dt               ! mesh and time resolution
  character(len=NF90_MAX_NAME)::dim_fire_x,dim_fire_y,dim_time ! dimension names, for info only
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

!!The function NF90_DEF_DIM adds a new dimension to an open netCDF dataset in 
!!define mode. It returns (as an argument) a dimension ID, given the netCDF ID,
!!the dimension name, and the dimension length. (From NetCDF docs)

call check(nf90_def_dim(ncid,dim_time,nf90_unlimited,dimid))
!if(compat_fire_grid)then
!  call check(nf90_def_dim(ncid,dim_fire_x,(info%natmx+1)*info%sr_x,dimid))
!  call check(nf90_def_dim(ncid,dim_fire_y,(info%natmy+1)*info%sr_y,dimid))
!else
  call check(nf90_def_dim(ncid,dim_fire_x,info%nfirex,dimid))
  call check(nf90_def_dim(ncid,dim_fire_y,info%nfirey,dimid))
!endif
!call check(nf90_def_dim(ncid,dim_atm_x,info%natmx,dimid))
!call check(nf90_def_dim(ncid,dim_atm_y,info%natmy,dimid))
!call check(nf90_def_dim(ncid,dim_atm_z,info%natmz,dimid))
!call check(nf90_def_dim(ncid,dim_atm_x_s,info%natmx+1,dimid))
!call check(nf90_def_dim(ncid,dim_atm_y_s,info%natmy+1,dimid))
!call check(nf90_def_dim(ncid,dim_atm_z_s,info%natmz+1,dimid))
call check(nf90_def_dim(ncid,'DateStrLen',12,dimid))

! define global attributes
!!call check(nf90_put_att(ncid,nf90_global,'DX',info%dx))
!!call check(nf90_put_att(ncid,nf90_global,'DY',info%dy))
call check(nf90_put_att(ncid,nf90_global,'DT',info%dt))
call check(nf90_put_att(ncid,nf90_global,'STANDALONE_DRIVER',1))

call check(nf90_enddef(ncid))
call check(nf90_close(ncid))

end subroutine create_dim_attr

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
call define_var(filename,var_lfn,dim_fire,unit_lfn,desc_lfn)
call define_var(filename,var_tign_g,dim_fire,unit_tign_g,desc_tign_g)
call define_var(filename,var_fgrnhfx,dim_fire,unit_fgrnhfx,desc_fgrnhfx)
call define_var(filename,var_fgrnqfx,dim_fire,unit_fgrnqfx,desc_fgrnqfx)
call define_var(filename,var_fuel_frac,dim_fire,unit_fuel_frac,desc_fuel_frac)

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
end subroutine define_var

!
!***
!

subroutine write_inputs(filename,input,info)
implicit none

! write simulation input file in a way compatible 
! with the wrf input file

!*** arguments
character(len=*),intent(in)::filename
type(input_vars_fire),intent(in)::input
type(grid_info),intent(in)::info

!*** local
integer::ncid,varid
integer,dimension(4)::s,c

!*** executable
call create_dim_attr(filename,info)
call define_var(filename,var_nfuel_cat,dim_fire,'','')
call define_var(filename,var_u,dim_atm_u,'','')
call define_var(filename,var_v,dim_atm_v,'','')
call define_var(filename,var_dzdxf,dim_fire,'','')
call define_var(filename,var_dzdyf,dim_fire,'','')
call define_var(filename,var_zsf,dim_fire,'','')

call ncopen(filename,nf90_write,ncid)

s(:)=1
c(:)=1
c(1)=info%nfirex
c(2)=info%nfirey

call check(nf90_inq_varid(ncid,var_nfuel_cat,varid))
call check(nf90_put_var(ncid,varid,input%nfuel_cat,start=s(1:3),count=c(1:3)))
call check(nf90_inq_varid(ncid,var_dzdxf,varid))
call check(nf90_put_var(ncid,varid,input%dzdxf,start=s(1:3),count=c(1:3)))
call check(nf90_inq_varid(ncid,var_dzdyf,varid))
call check(nf90_put_var(ncid,varid,input%dzdyf,start=s(1:3),count=c(1:3)))
call check(nf90_inq_varid(ncid,var_zsf,varid))
call check(nf90_put_var(ncid,varid,input%zsf,start=s(1:3),count=c(1:3)))

!c(1)=info%natmx+1
!c(2)=info%natmy

!call check(nf90_inq_varid(ncid,var_uf,varid))
!call check(nf90_put_var(ncid,varid,input%uf,start=s,count=c))

!c(1)=info%natmx
!c(2)=info%natmy+1

!call check(nf90_inq_varid(ncid,var_vf,varid))
!call check(nf90_put_var(ncid,varid,input%vf,start=s,count=c))

call check(nf90_close(ncid))

end subroutine write_inputs

!
!***
!

subroutine write_vars(filename,output,time)
!*** append variables to an output file (extending by the unlimited time
!    dimension)
implicit none

!*** arguments
character(len=*),intent(in)::filename
type(output_vars_fire),intent(in)::output
real,intent(in)::time

!*** local
integer::ncid,tstep,dimid,varid
integer,dimension(3)::s,c
character(len=12)::timestr

!*** executable

! open the file
call ncopen(filename,nf90_write,ncid)

! get the current number of timeslices already written
call check(nf90_inq_dimid(ncid,dim_time,dimid))
call check(nf90_inquire_dimension(ncid,dimid,len=tstep))

! write out the current simulation time
call check(nf90_inq_varid(ncid,'Times',varid))
write(timestr,'(E12.3)')time
call check(nf90_put_var(ncid,varid,timestr,start=(/1,tstep+1/),count=(/12,1/)))

! set up start and count variables for nf90_put_var so that it writes to the
! correct time slice and write data for each variable
s(:)=1
c(:)=0
s(3)=tstep+1
c(3)=1
c(1:2)=(/size(output%lfn,1),size(output%lfn,2)/)
call check(nf90_inq_varid(ncid,var_lfn,varid))
call check(nf90_put_var(ncid,varid,output%lfn,start=s,count=c))
c(1:2)=(/size(output%tign,1),size(output%tign,2)/)
call check(nf90_inq_varid(ncid,var_tign_g,varid))
call check(nf90_put_var(ncid,varid,output%tign,start=s,count=c))
c(1:2)=(/size(output%fgrnhfx,1),size(output%fgrnhfx,2)/)
call check(nf90_inq_varid(ncid,var_fgrnhfx,varid))
call check(nf90_put_var(ncid,varid,output%fgrnhfx,start=s,count=c))
call check(nf90_inq_varid(ncid,var_fgrnqfx,varid))
call check(nf90_put_var(ncid,varid,output%fgrnqfx,start=s,count=c))
call check(nf90_inq_varid(ncid,var_fuel_frac,varid))
call check(nf90_put_var(ncid,varid,output%fuel_frac,start=s,count=c))

! close file
call latch
call check(nf90_close(ncid))
end subroutine write_vars

!
!*** READING 
!

subroutine set_fire_info_from_file(filename,info)
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
end subroutine set_fire_info_from_file

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

subroutine read_var_dims(ncid,varname,mdims,ndims,dims,sr,dimnames,dimids)
! get variable dimensions from a file
! arguments
integer, intent(in):: ncid             ! open netcdf file
character(len=*), intent(in):: varname ! variable name
integer, intent(in):: mdims            ! maximal number of dimensions
integer, intent(out)::ndims            ! number of dimensions of this variable
integer, intent(out)::dims(mdims)      ! the dimensions
integer, intent(out), optional::sr(mdims)! refinement ratios
character(len=*),intent(out),optional::dimnames(mdims) ! dimension names
integer,intent(out),optional::dimids(mdims) ! dimension ids
! local
integer::varid,dimid,xtype,idimids(mdims),idim,i,ratio,staglen,stagid,dimlen
character(len=NF90_MAX_NAME)::dimname,stagname
character(len=128)::msg
character(len=*), parameter:: subgrid='_subgrid'
! executable
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
    call check(nf90_inquire_dimension(ncid, idimids(idim), dimname, dimlen))
    ratio=0
    i=index(dimname,subgrid,.true.)
    !print *,trim(dimname),subgrid,i,len(subgrid),len_trim(dimname)
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

subroutine read_one_fire_var(ncid,info,iframe,varname,v)
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

end subroutine read_one_fire_var

!
!***
!

subroutine read_fire_vars(filename,info,iframe,grid)
! read all variables from input file
implicit none

!*** arguments
character(len=*),intent(in)::filename ! the input file
type(grid_info),intent(in)::info      ! dimensions
integer, intent(in):: iframe          ! number of frame in the file
type(domain),intent(inout)::grid      ! the mother of all arrays

!*** local
integer::ncid,varid,ierr
integer,dimension(4)::s,c

!*** executable
write(*,'(3a,i4)')'reading file ',trim(filename),' frame ',iframe 
call ncopen(filename,nf90_nowrite,ncid)

call read_one_fire_var(ncid,info,iframe,var_nfuel_cat,grid%nfuel_cat) 
call read_one_fire_var(ncid,info,iframe,var_dzdxf,grid%dzdxf) 
call read_one_fire_var(ncid,info,iframe,var_dzdyf,grid%dzdyf) 
call read_one_fire_var(ncid,info,iframe,var_uf,grid%uf) 
call read_one_fire_var(ncid,info,iframe,var_vf,grid%vf) 
call read_one_fire_var(ncid,info,iframe,var_lfn,grid%lfn) 
call read_one_fire_var(ncid,info,iframe,var_tign_g,grid%tign_g) 
call read_one_fire_var(ncid,info,iframe,var_fuel_frac,grid%fuel_frac) 

call check(nf90_close(ncid))
end subroutine read_fire_vars

subroutine check(ncerr,msg)
implicit none
integer,intent(in)::ncerr
character(len=*), optional, intent(in)::msg
if(ncerr.ne.nf90_noerr)then
  write(6,'(2a)')"NetCDF error: ",trim(nf90_strerror(ncerr))
  if(present(msg))then
    call crash(msg)
  else
    call crash("NETCDF ERROR")
  endif
endif
end subroutine check

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
