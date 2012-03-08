
module moisture_util
use module_fr_sfire_util, only : crash
use esmf_mod
use netcdf
implicit none

integer,parameter::TIMESTRLEN=19,nfmc=5
logical,parameter::smalloutput=.false.
integer,save:: ims,ime,jms,jme
integer,save::its,ite,jts,jte
logical,private,save::initialized=.false.

type ncvars
    real,pointer,dimension(:,:,:)::fmc_gc,fmc_equi,fmc_tend
    real,pointer,dimension(:,:)::t2,q2,psfc,rainc,rainnc, &
               t2_old,q2_old,psfc_old,rain_old
    type(ESMF_Time)::time
end type

type ncfile
    integer::ntime
    type(ESMF_Time),dimension(:),pointer::times
    integer::ncid
end type

contains

subroutine initialize_and_check(filename,wrffile)
! open a file and initialize an ncfile type
! if module is not initialized, then initialize
! otherwise check that sizes are consistent
character(len=*),intent(in)::filename
type(ncfile),intent(out)::wrffile
integer::dimidx,dimidy,dimidtime,dimidfuel
integer::varidtime
integer::nx,ny,nfuel,i,rc
character(len=TIMESTRLEN),dimension(:),allocatable::timestr

call ESMF_Initialize(defaultCalKind=ESMF_CAL_GREGORIAN)

call check(nf90_open(filename,nf90_nowrite,wrffile%ncid))
call check(nf90_inq_dimid(wrffile%ncid,'west_east',  dimidx))
call check(nf90_inq_dimid(wrffile%ncid,'south_north',dimidy))
call check(nf90_inq_dimid(wrffile%ncid,'Time',dimidtime))
call check(nf90_inquire_dimension(wrffile%ncid,dimidx,len=nx))
call check(nf90_inquire_dimension(wrffile%ncid,dimidy,len=ny))
call check(nf90_inquire_dimension(wrffile%ncid,dimidtime,len=wrffile%ntime))

if(.not.initialized)then
    ims=1
    ime=nx
    jms=1
    jme=ny
    its=ims
    ite=ime
    jts=jms
    jte=jme
    initialized=.true.
else
    if( (nx.ne.ime-ims+1) .or. &
        (ny.ne.jme-jms+1) ) then
        call crash('Incompatible file: '//trim(filename))
    endif
endif

allocate(wrffile%times(wrffile%ntime))
allocate(timestr(wrffile%ntime))

print*,wrffile%ntime
call check(nf90_inq_varid(wrffile%ncid,'Times',varidtime))
call check(nf90_get_var(wrffile%ncid,varidtime,timestr))

do i=1,wrffile%ntime
    call parse_wrf_time(timestr(i),wrffile%times(i))
enddo

deallocate(timestr)

end subroutine initialize_and_check

subroutine alldone()
implicit none
call ESMF_Finalize()
end subroutine alldone

subroutine destroy_file(wrffile)
implicit none
type(ncfile),intent(inout)::wrffile
call check(nf90_close(wrffile%ncid))
deallocate(wrffile%times)
wrffile%ntime=0
end subroutine destroy_file

subroutine initialize_vars(vars)
implicit none
type(ncvars),intent(inout)::vars
if(.not.initialized)then
  call crash('Module not initialized')
endif
allocate(vars%fmc_gc(ims:ime,1:nfmc,jms:jme))
allocate(vars%fmc_equi(ims:ime,1:nfmc,jms:jme))
allocate(vars%fmc_tend(ims:ime,1:nfmc,jms:jme))
allocate(vars%t2(ims:ime,jms:jme))
allocate(vars%q2(ims:ime,jms:jme))
allocate(vars%psfc(ims:ime,jms:jme))
allocate(vars%rainc(ims:ime,jms:jme))
allocate(vars%rainnc(ims:ime,jms:jme))
allocate(vars%t2_old(ims:ime,jms:jme))
allocate(vars%q2_old(ims:ime,jms:jme))
allocate(vars%psfc_old(ims:ime,jms:jme))
allocate(vars%rain_old(ims:ime,jms:jme))
end subroutine initialize_vars

subroutine destroy_vars(vars)
implicit none
type(ncvars),intent(inout)::vars
deallocate(vars%fmc_gc)
deallocate(vars%fmc_equi)
deallocate(vars%fmc_tend)
deallocate(vars%t2)
deallocate(vars%q2)
deallocate(vars%psfc)
deallocate(vars%rainc)
deallocate(vars%rainnc)
deallocate(vars%t2_old)
deallocate(vars%q2_old)
deallocate(vars%psfc_old)
deallocate(vars%rain_old)
end subroutine destroy_vars

subroutine create_output(filename)
implicit none
character(len=*),intent(in)::filename
integer::ncid,nxid,nyid,timeid,fuelid,dateid
integer::tmp

! create dataset
call check(nf90_create(filename,nf90_clobber,ncid))

! create dimensions
call check(nf90_def_dim(ncid,'west_east',ime-ims+1,nxid))
call check(nf90_def_dim(ncid,'south_north',jme-jms+1,nyid))
call check(nf90_def_dim(ncid,'Time',nf90_unlimited,timeid))
call check(nf90_def_dim(ncid,'fuel_moisture_classes_stag',nfmc,fuelid))
call check(nf90_def_dim(ncid,'DateStrLen',TIMESTRLEN,dateid))

! create variables
call check(nf90_def_var(ncid,'Times',NF90_CHAR,(/dateid,timeid/),tmp))
call check(nf90_def_var(ncid,'FMC_GC',NF90_REAL,(/nxid,nyid,fuelid,timeid/),tmp))
if(.not.smalloutput)then
    call check(nf90_def_var(ncid,'FMC_EQUI',NF90_REAL,(/nxid,nyid,fuelid,timeid/),tmp))
    call check(nf90_def_var(ncid,'FMC_TEND',NF90_REAL,(/nxid,nyid,fuelid,timeid/),tmp))
    call check(nf90_def_var(ncid,'T2',NF90_REAL,(/nxid,nyid,timeid/),tmp))
    call check(nf90_def_var(ncid,'Q2',NF90_REAL,(/nxid,nyid,timeid/),tmp))
    call check(nf90_def_var(ncid,'PSFC',NF90_REAL,(/nxid,nyid,timeid/),tmp))
    call check(nf90_def_var(ncid,'RAIN',NF90_REAL,(/nxid,nyid,timeid/),tmp))
endif
call check(nf90_close(ncid))
end subroutine create_output

subroutine write_output(filename,vars)
implicit none
character(len=*),intent(in)::filename
type(ncvars),intent(in)::vars
integer::ncid,varid,dimid,itime
character(len=TIMESTRLEN)::timestr
real,dimension(:,:,:),allocatable::tmp

allocate(tmp(its:ite,jts:jte,1:nfmc))
print*,'writing output to '//trim(filename)
call check(nf90_open(filename,NF90_WRITE,ncid))
call check(nf90_inq_dimid(ncid,'Time',dimid))
call check(nf90_inquire_dimension(ncid,dimid,len=itime))
itime=itime+1

call check(nf90_inq_varid(ncid,'Times',varid))
call ESMF_TimeGet(vars%time,timeString=timestr)
call check(nf90_put_var(ncid,varid,timestr, &
             start=(/1,itime/),count=(/TIMESTRLEN,1/)))

call check(nf90_inq_varid(ncid,'FMC_GC',varid))
call transpose_var(vars%fmc_gc,tmp)
call check(nf90_put_var(ncid,varid,tmp, &
             start=(/its,jts,1,itime/),count=(/ite,jte,nfmc,1/)))

if(.not.smalloutput)then

    call check(nf90_inq_varid(ncid,'FMC_EQUI',varid))
    call transpose_var(vars%fmc_equi,tmp)
    call check(nf90_put_var(ncid,varid,tmp, &
                 start=(/its,jts,1,itime/),count=(/ite,jte,nfmc,1/)))

    call check(nf90_inq_varid(ncid,'FMC_TEND',varid))
    call transpose_var(vars%fmc_tend,tmp)
    call check(nf90_put_var(ncid,varid,tmp, &
                 start=(/its,jts,1,itime/),count=(/ite,jte,nfmc,1/)))

    call check(nf90_inq_varid(ncid,'T2',varid))
    call check(nf90_put_var(ncid,varid,vars%t2, &
                 start=(/its,jts,itime/),count=(/ite,jte,1/)))

    call check(nf90_inq_varid(ncid,'Q2',varid))
    call check(nf90_put_var(ncid,varid,vars%q2, &
                 start=(/its,jts,itime/),count=(/ite,jte,1/)))

    call check(nf90_inq_varid(ncid,'PSFC',varid))
    call check(nf90_put_var(ncid,varid,vars%psfc, &
                 start=(/its,jts,itime/),count=(/ite,jte,1/)))

    call check(nf90_inq_varid(ncid,'RAIN',varid))
    call check(nf90_put_var(ncid,varid,vars%rainc+vars%rainnc, &
                 start=(/its,jts,itime/),count=(/ite,jte,1/)))
endif
         
call check(nf90_close(ncid))
deallocate(tmp)
end subroutine write_output

subroutine read_file(wrffile,itime,vars)
implicit none
type(ncfile),intent(in)::wrffile
integer,intent(in)::itime
type(ncvars),intent(inout)::vars
integer::varid,i,j,k
real,dimension(:,:,:),allocatable::tmp

vars%time=wrffile%times(itime)

!allocate(tmp(its:ite,jts:jte,nfmc))
!call check(nf90_inq_varid(wrffile%ncid,'FMC_GC',varid))
!call check(nf90_get_var(wrffile%ncid,varid,tmp, &
!        start=(/its,jts,1,itime/), count=(/ite,jte,nfmc,1/)))
!call transpose_var(tmp,vars%fmc_gc)

call check(nf90_inq_varid(wrffile%ncid,'T2',varid))
call check(nf90_get_var(wrffile%ncid,varid,vars%t2, &
        start=(/its,jts,itime/), count=(/ite,jte,1/)))

call check(nf90_inq_varid(wrffile%ncid,'Q2',varid))
call check(nf90_get_var(wrffile%ncid,varid,vars%q2, &
        start=(/its,jts,itime/), count=(/ite,jte,1/)))

call check(nf90_inq_varid(wrffile%ncid,'PSFC',varid))
call check(nf90_get_var(wrffile%ncid,varid,vars%psfc, &
        start=(/its,jts,itime/), count=(/ite,jte,1/)))

call check(nf90_inq_varid(wrffile%ncid,'RAINC',varid))
call check(nf90_get_var(wrffile%ncid,varid,vars%rainc, &
        start=(/its,jts,itime/), count=(/ite,jte,1/)))

call check(nf90_inq_varid(wrffile%ncid,'RAINNC',varid))
call check(nf90_get_var(wrffile%ncid,varid,vars%rainnc, &
        start=(/its,jts,itime/), count=(/ite,jte,1/)))
!deallocate(tmp)
end subroutine read_file

subroutine transpose_var(A,B)
implicit none
real,dimension(:,:,:),intent(in)::A
real,dimension(:,:,:),intent(out)::B
integer::i,j,k
do i=1,size(A,1)
    do j=1,size(A,2)
        do k=1,size(A,3)
            B(i,k,j)=A(i,j,k)
        enddo
    enddo
enddo
end subroutine transpose_var

subroutine parse_wrf_time(wrfstr,time)
! parse a wrf time string into an esmf time class
! example wrf time string: "2011-01-01_00:00:00"
implicit none
character(len=TIMESTRLEN),intent(in)::wrfstr
type(ESMF_Time),intent(out)::time

integer::year,month,day,hour,minute,second
character(len=1)::a1,a2,a3,a4,a5

read(wrfstr,'(I04,A1,I02,A1,I02,A1,I02,A1,I02,A1,I02)') &
       year,a1,month,a2,day,a3,hour,a4,minute,a5,second
call ESMF_TimeSet(time,YY=year,MM=month,DD=day,H=hour,M=minute,S=second)
end subroutine parse_wrf_time

subroutine check(status)
integer,intent(in)::status
if(status.ne.nf90_noerr) then
    call crash(trim(nf90_strerror(status)))
endif
end subroutine check

end module moisture_util

! standalone moisture model test
program moisture_main
use moisture_util
use module_fr_sfire_phys, only: advance_moisture, init_fuel_cats, moisture_classes
use module_fr_sfire_util, only: fire_print_msg 
implicit none

real, dimension(:,:,:),allocatable::fmc_gc,fmc_equi,fmc_tend
real, dimension(:,:),pointer::t2,q2,psfc,t2_old,q2_old,psfc_old, &
   rainc,rainnc,rain_old
integer::nsteps=100
logical::initialize
integer::fmoist_init=2
real::moisture_dt,dt
integer::istep,i

integer :: numarg,ifile,ii,nfile,s,sn,sd
character(len=120) :: wrfoutfname
character(len=*),parameter :: outputfname='moisture.nc'
character(len=64)::timediffstr

type(ncfile),dimension(:),allocatable::wrffile
type(ncvars)::vars
type(ESMF_Time)::oldtime,timenow
type(ESMF_TimeInterval)::timedt

!integer,external :: iargc

! parse commandline argument for wrfout file name
numarg=iargc()

if(numarg .lt. 1)then
    print*, 'This program takes at least one argument, the wrfout files to run the moisture model from.'
    call abort()
endif

allocate(wrffile(numarg))

nfile=0
do i=1,numarg
    call getarg(i,wrfoutfname)
    print*,'using: '//trim(wrfoutfname)
    call initialize_and_check(wrfoutfname,wrffile(i))
    nfile=nfile+1
enddo
call initialize_vars(vars)
oldtime=wrffile(i)%times(1)
timenow=oldtime
nsteps=99999

call create_output(outputfname)

!*** executable
fire_print_msg = 2

call init_fuel_cats(.true.)
print *,moisture_classes,' moisture classes'

ifile=1
ii=0
do istep=1,nsteps
    oldtime=timenow
    ii=ii+1 
    if(ii.gt.wrffile(ifile)%ntime)then
        ifile=ifile+1
        ii=1
    endif

    if(ifile.gt.nfile) goto 99
    print*,'reading file ',ifile,' of ',nfile
    print*,'time step ',ii,' of ',wrffile(ifile)%ntime
    call read_file(wrffile(ifile),ii,vars)
    timenow=wrffile(ifile)%times(ii)
   

    if(istep.eq.1)then
        dt=0.  ! required on initialization
        initialize=.true.
    else
        timedt=timenow-oldtime
        call ESMF_TimeIntervalGet(timedt,S=s,sn=sn,sd=sd,TimeString=timediffstr)
        !print*,trim(timediffstr),s,sn,sd
        if(sd.gt.0)then
            dt=dble(s)+dble(sn)/dble(sd)
        else
            dt=dble(s)
        endif
        initialize=.false.
    endif
 
    call advance_moisture(    &
        initialize,                 & ! initialize timestepping. true on the first call at time 0, then false
        fmoist_init,                & ! initial moisture: 1=fuelmc_g, 2=equilibrium, else = none
        ims,ime,  jms,jme,          & ! memory dimensions
        its,ite,  jts,jte,          & ! tile dimensions
        nfmc,                       & ! dimension of moisture fields
        dt,                & ! timestep = time step time elapsed from the last call
        vars%rainc, vars%rainnc,              & ! accumulated rain
        vars%t2, vars%q2, vars%psfc,               & ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
        vars%rain_old,                   & ! previous value of accumulated rain
        vars%t2_old, vars%q2_old, vars%psfc_old,   & ! previous values of the atmospheric state at surface
        vars%fmc_gc,                     & ! fuel moisture by class, updated
        vars%fmc_equi,                   & ! fuel moisture equilibrium by class, for diagnostics only
        vars%fmc_tend                    & ! fuel moisture tendency by classe, for diagnostics only
        )
    call write_output(outputfname,vars)

enddo
 99 continue
call destroy_vars(vars)
do i=1,nfile
    call destroy_file(wrffile(i))
enddo
call alldone()
end program moisture_main
