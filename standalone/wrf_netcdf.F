!*** A simple output module for wrf-fire standalone that produces output 
!    files that are somewhat compatible with wrf output files.

module wrf_netcdf
use netcdf
use module_fr_sfire_util , only : crash
implicit none

! control whether the dimensions of the fire grid variables should have 
! the same (incorrect) sizes that wrf outputs for compatibility
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
                              var_vx='FX',                      &
                              unit_vx='',                       &
                              desc_vx='',                       &
                              var_vy='FY',                      &
                              unit_vy='',                       &
                              desc_vy='',                       &
                              var_grnhfx='FGRNHFX',             &
                              unit_grnhfx='',                   &
                              desc_grnhfx=''

! grid information structure
type grid_info
  integer::nfirex,nfirey,      & ! fire grid size
           natmx,natmy,natmz,  & ! atmospheric grid size
           sr_x,sr_y             ! subgrid refinement
  real::dx,dy,dt                 ! mesh and time resolution (in terms of the atmospheric grid)
end type

! pointer to all variables being written (to simplify interface)
type output_vars
  real,dimension(:,:),pointer::lfn,tign,vx,vy,grnhfx
end type

contains

subroutine create_file(filename,grid)
!*** Create a file containing meta-data suitable for output of this simulation.
!    Only creates dimensions, variables, and attributes... does not populate
!    data.  Squash file if it already exists.
implicit none
character(len=*),intent(in)::filename  ! filename to create
type(grid_info),intent(in)::grid       ! grid information structure
integer::ncid,dimid,timeid,strlenid,varid

! create an empty file
call check(nf90_create(filename,nf90_clobber,ncid))

! define all dimensions
call check(nf90_def_dim(ncid,dim_time,nf90_unlimited,dimid))
timeid=dimid
if(compat_fire_grid)then
  call check(nf90_def_dim(ncid,dim_fire_x,(grid%natmx+1)*grid%sr_x,dimid))
  call check(nf90_def_dim(ncid,dim_fire_y,(grid%natmy+1)*grid%sr_y,dimid))
else
  call check(nf90_def_dim(ncid,dim_fire_x,grid%nfirex,dimid))
  call check(nf90_def_dim(ncid,dim_fire_y,grid%nfirey,dimid))
endif
call check(nf90_def_dim(ncid,dim_atm_x,grid%natmx,dimid))
call check(nf90_def_dim(ncid,dim_atm_y,grid%natmy,dimid))
call check(nf90_def_dim(ncid,dim_atm_z,grid%natmz,dimid))
call check(nf90_def_dim(ncid,dim_atm_x_s,grid%natmx+1,dimid))
call check(nf90_def_dim(ncid,dim_atm_y_s,grid%natmy+1,dimid))
call check(nf90_def_dim(ncid,dim_atm_z_s,grid%natmz+1,dimid))
call check(nf90_def_dim(ncid,'DateStrLen',12,dimid))
strlenid=dimid

! define global attributes
call check(nf90_put_att(ncid,nf90_global,'DX',grid%dx))
call check(nf90_put_att(ncid,nf90_global,'DY',grid%dy))
call check(nf90_put_att(ncid,nf90_global,'DT',grid%dt))
call check(nf90_put_att(ncid,nf90_global,'STANDALONE_DRIVER',1))

! define a timekeeping variable
call check(nf90_def_var(ncid,'Times',nf90_char,(/strlenid,timeid/),varid))

! close file
call check(nf90_enddef(ncid))
call check(nf90_close(ncid))

! create all of the output variables
call define_var(filename,var_lfn,dim_fire,unit_lfn,desc_lfn)
call define_var(filename,var_tign,dim_fire,unit_tign,desc_tign)
call define_var(filename,var_vx,dim_fire,unit_vx,desc_vx)
call define_var(filename,var_vy,dim_fire,unit_vy,desc_vy)
call define_var(filename,var_grnhfx,dim_fire,unit_grnhfx,desc_grnhfx)
end subroutine create_file

subroutine define_var(filename,varname,dims,units,description)
!*** define a variable in a netcdf data set, the file is assumed to exist and
!    have valid meta-data (as created by create_file)
implicit none
character(len=*),intent(in)::filename,varname  ! create variable varname in filename
character(len=*),dimension(:),intent(in)::dims ! the dimension names of the variable
character(len=*),intent(in)::units,description ! attributes created by wrf (not used at the moment)
integer::ncid,i,varid
integer,dimension(4)::dimids
character(len=*),parameter::memorder='XYZ'
character(len=3)::stag

! open the file
call check(nf90_open(filename,nf90_write,ncid))

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

subroutine write_vars(filename,output,time)
!*** append variables to an output file (extending by the unlimited time
!    dimension)
implicit none
character(len=*),intent(in)::filename
type(output_vars),intent(in)::output
real,intent(in)::time
integer::ncid,tstep,dimid,varid
integer,dimension(3)::s,c
character(len=12)::timestr

! open the file
call check(nf90_open(filename,nf90_write,ncid))

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
c(1:2)=(/size(output%vx,1),size(output%vx,2)/)
call check(nf90_inq_varid(ncid,var_vx,varid))
call check(nf90_put_var(ncid,varid,output%vx,start=s,count=c))
c(1:2)=(/size(output%vy,1),size(output%vy,2)/)
call check(nf90_inq_varid(ncid,var_vy,varid))
call check(nf90_put_var(ncid,varid,output%vy,start=s,count=c))
c(1:2)=(/size(output%grnhfx,1),size(output%grnhfx,2)/)
call check(nf90_inq_varid(ncid,var_grnhfx,varid))
call check(nf90_put_var(ncid,varid,output%grnhfx,start=s,count=c))

! close file
call check(nf90_close(ncid))
end subroutine write_vars

subroutine check(ncerr)
implicit none
integer,intent(in)::ncerr
if(ncerr.ne.nf90_noerr)then
  print*,"Error calling NetCDF subroutine"
  print*,trim(nf90_strerror(ncerr))
  call crash("NETCDF ERROR")
endif
end subroutine check

end module wrf_netcdf
