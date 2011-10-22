!*** A simple output module for wrf-fire standalone that produces output  READING 
!    files and reads input files that are somewhat compatible with wrf files
!

module wrf_netcdf
use netcdf
use module_fr_sfire_util , only : crash, interpolate_2d, continue_at_boundary
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
                              var_tign='TIGN_G',                &
                              unit_tign='',                     &
                              desc_tign='',                     &
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
           natmx,natmy,natmz,  & ! atmospheric grid size
           sr_x,sr_y             ! subgrid refinement
  real::dx,dy,dt                 ! mesh and time resolution (in terms of the atmospheric grid)
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
if(compat_fire_grid)then
  call check(nf90_def_dim(ncid,dim_fire_x,(info%natmx+1)*info%sr_x,dimid))
  call check(nf90_def_dim(ncid,dim_fire_y,(info%natmy+1)*info%sr_y,dimid))
else
  call check(nf90_def_dim(ncid,dim_fire_x,info%nfirex,dimid))
  call check(nf90_def_dim(ncid,dim_fire_y,info%nfirey,dimid))
endif
call check(nf90_def_dim(ncid,dim_atm_x,info%natmx,dimid))
call check(nf90_def_dim(ncid,dim_atm_y,info%natmy,dimid))
call check(nf90_def_dim(ncid,dim_atm_z,info%natmz,dimid))
call check(nf90_def_dim(ncid,dim_atm_x_s,info%natmx+1,dimid))
call check(nf90_def_dim(ncid,dim_atm_y_s,info%natmy+1,dimid))
call check(nf90_def_dim(ncid,dim_atm_z_s,info%natmz+1,dimid))
call check(nf90_def_dim(ncid,'DateStrLen',12,dimid))

! define global attributes
call check(nf90_put_att(ncid,nf90_global,'DX',info%dx))
call check(nf90_put_att(ncid,nf90_global,'DY',info%dy))
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
call define_var(filename,var_tign,dim_fire,unit_tign,desc_tign)
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

c(1)=info%natmx+1
c(2)=info%natmy

call check(nf90_inq_varid(ncid,var_uf,varid))
call check(nf90_put_var(ncid,varid,input%uf,start=s,count=c))

c(1)=info%natmx
c(2)=info%natmy+1

call check(nf90_inq_varid(ncid,var_vf,varid))
call check(nf90_put_var(ncid,varid,input%vf,start=s,count=c))

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
call check(nf90_inq_varid(ncid,var_tign,varid))
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

subroutine set_info_from_file(filename,info)
! get grid sizes from input file
implicit none

!*** arguments
character(len=*),intent(in)::filename
type(grid_info),intent(inout)::info

!*** local
integer::ncid,it
integer,dimension(4)::fdimid,adimid

!*** executable
call ncopen(filename,nf90_nowrite,ncid)
call check(nf90_inq_dimid(ncid,dim_time,fdimid(3)))
adimid(4)=fdimid(3)
call check(nf90_inq_dimid(ncid,dim_fire_x,fdimid(1)))
call check(nf90_inq_dimid(ncid,dim_fire_y,fdimid(2)))
call check(nf90_inq_dimid(ncid,dim_atm_x, adimid(1)))
call check(nf90_inq_dimid(ncid,dim_atm_y, adimid(2)))
call check(nf90_inq_dimid(ncid,dim_atm_z, adimid(3)))

call check(nf90_inquire_dimension(ncid,fdimid(3),len=it))
!if(it.ne.1)then
!  call crash('invalid number of time steps in input file, must be 1')
!endif
call check(nf90_inquire_dimension(ncid,fdimid(2),len=info%nfirey))
call check(nf90_inquire_dimension(ncid,fdimid(1),len=info%nfirex))
call check(nf90_inquire_dimension(ncid,adimid(2),len=info%natmy))
call check(nf90_inquire_dimension(ncid,adimid(1),len=info%natmx))
call check(nf90_inquire_dimension(ncid,adimid(3),len=info%natmz))

call check(nf90_get_att(ncid,nf90_global,'DX',info%dx))
call check(nf90_get_att(ncid,nf90_global,'DY',info%dy))
call check(nf90_get_att(ncid,nf90_global,'DT',info%dt))

if(compat_fire_grid)then
  info%sr_x=info%nfirex/(info%natmx+1)
  info%sr_y=info%nfirey/(info%natmy+1)
  if( (info%natmx+1)*info%sr_x .ne. info%nfirex .or. &
      (info%natmy+1)*info%sr_y .ne. info%nfirey)then
      call crash('invalid dimensions in input file or compat_fire_grid set incorrectly')
  endif
  info%nfirex=info%natmx*info%sr_x
  info%nfirey=info%natmy*info%sr_y
else
  info%sr_x=info%nfirex/info%natmx
  info%sr_y=info%nfirey/info%natmy
  if( info%natmx*info%sr_x .ne. info%nfirex .or. &
      info%natmy*info%sr_y .ne. info%nfirey)then
      call crash('invalid dimensions in input file or compat_fire_grid set incorrectly')
  endif
endif
call check(nf90_close(ncid))
end subroutine set_info_from_file

!
!***
!

subroutine read_vars_fire(filename,info,input)
! read all variables from input file
implicit none

!*** arguments
character(len=*),intent(in)::filename
type(input_vars_fire),intent(inout)::input
type(grid_info),intent(in)::info

!*** local
integer::ncid,varid,ierr
integer,dimension(4)::s,c

!*** executable
call ncopen(filename,nf90_nowrite,ncid)
s(:)=1
c(:)=1
c(1)=info%nfirex
c(2)=info%nfirey
call check(nf90_inq_varid(ncid,var_nfuel_cat,varid))
call check(nf90_get_var(ncid,varid,input%nfuel_cat,start=s(1:3),count=c(1:3)))
call check(nf90_inq_varid(ncid,var_dzdxf,varid))
call check(nf90_get_var(ncid,varid,input%dzdxf,start=s(1:3),count=c(1:3)))
call check(nf90_inq_varid(ncid,var_dzdyf,varid))
call check(nf90_get_var(ncid,varid,input%dzdyf,start=s(1:3),count=c(1:3)))
call check(nf90_inq_varid(ncid,var_uf,varid))
call check(nf90_get_var(ncid,varid,input%uf,start=s(1:3),count=c(1:3)))
call check(nf90_inq_varid(ncid,var_vf,varid))
call check(nf90_get_var(ncid,varid,input%vf,start=s(1:3),count=c(1:3)))
call check(nf90_inq_varid(ncid,var_lfn,varid))
call check(nf90_get_var(ncid,varid,input%lfn,start=s(1:3),count=c(1:3)))
call check(nf90_inq_varid(ncid,var_tign,varid))
call check(nf90_get_var(ncid,varid,input%tign,start=s(1:3),count=c(1:3)))
call check(nf90_inq_varid(ncid,var_fuel_frac,varid))
call check(nf90_get_var(ncid,varid,input%fuel_frac,start=s(1:3),count=c(1:3)))

s(:)=1

!c(1)=info%natmx
!c(2)=info%natmy
!c(3)=1
!c(4)=1
!
!call check(nf90_inq_varid(ncid,var_z0,varid))
!call check(nf90_get_var(ncid,varid,input%z0,start=s,count=c))
!
!c(1)=info%natmx
!c(2)=info%natmy
!c(3)=info%natmz+1
!c(4)=1
!
!call check(nf90_inq_varid(ncid,var_ph,varid))
!call check(nf90_get_var(ncid,varid,input%ph,start=s,count=c))
!
!c(1)=info%natmx
!c(2)=info%natmy
!c(3)=info%natmz+1
!c(4)=1
!
!call check(nf90_inq_varid(ncid,var_phb,varid))
!call check(nf90_get_var(ncid,varid,input%phb,start=s,count=c))
!
!c(1)=info%natmx+1
!c(2)=info%natmy
!c(3)=info%natmz
!c(4)=1
!
!call check(nf90_inq_varid(ncid,var_u,varid))
!call check(nf90_get_var(ncid,varid,input%u,start=s,count=c))
!
!c(1)=info%natmx
!c(2)=info%natmy+1
!c(3)=info%natmz
!c(4)=1
!
!call check(nf90_inq_varid(ncid,var_v,varid))
!call check(nf90_get_var(ncid,varid,input%v,start=s,count=c))

call check(nf90_close(ncid))
end subroutine read_vars_fire

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
