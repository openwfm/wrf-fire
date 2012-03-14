
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

subroutine read_file(wrffile,atime,vars,ierr)
implicit none
type(ncfile),intent(in)::wrffile
integer,intent(in)::atime
type(ncvars),intent(inout)::vars
integer,intent(out)::ierr
integer::varid,i,j,k,itime
real,dimension(:,:,:),allocatable::tmp

if(atime.gt.0)then
    itime=atime
else
    itime=size(wrffile%times)
endif
vars%time=wrffile%times(itime)

if(nf90_inq_varid(wrffile%ncid,'FMC_GC',varid).eq.0)then
    allocate(tmp(its:ite,jts:jte,nfmc))
    call check(nf90_get_var(wrffile%ncid,varid,tmp, &
            start=(/its,jts,1,itime/), count=(/ite,jte,nfmc,1/)))
    call transpose_var(tmp,vars%fmc_gc)
    deallocate(tmp)
else
    vars%fmc_gc=0.
endif
call check(nf90_inq_varid(wrffile%ncid,'T2',varid))
call check(nf90_get_var(wrffile%ncid,varid,vars%t2, &
        start=(/its,jts,itime/), count=(/ite,jte,1/)))

call check(nf90_inq_varid(wrffile%ncid,'Q2',varid))
call check(nf90_get_var(wrffile%ncid,varid,vars%q2, &
        start=(/its,jts,itime/), count=(/ite,jte,1/)))

call check(nf90_inq_varid(wrffile%ncid,'PSFC',varid))
call check(nf90_get_var(wrffile%ncid,varid,vars%psfc, &
        start=(/its,jts,itime/), count=(/ite,jte,1/)))

if(nf90_inq_varid(wrffile%ncid,'RAINC',varid).eq.0)then
    call check(nf90_get_var(wrffile%ncid,varid,vars%rainc, &
            start=(/its,jts,itime/), count=(/ite,jte,1/)))
    
    call check(nf90_inq_varid(wrffile%ncid,'RAINNC',varid))
    call check(nf90_get_var(wrffile%ncid,varid,vars%rainnc, &
            start=(/its,jts,itime/), count=(/ite,jte,1/)))
else
    call check(nf90_inq_varid(wrffile%ncid,'RAIN',varid))
    call check(nf90_get_var(wrffile%ncid,varid,vars%rainc, &
            start=(/its,jts,itime/), count=(/ite,jte,1/)))
    vars%rainnc(:,:)=0.
endif
if(all(vars%t2.eq.0).or.all(vars%q2.eq.0).or.all(vars%psfc.eq.0))then
    ierr=1
else
    ierr=0
endif
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

subroutine get_next_timestep(files,ifile,istep)
! choose a file and time step within the file to use
! for the next call to the moisture model
! 
! This assumes files are in sequential order, and 
! allows for reanalysis runs where there may be
! time step overlaps from one file to the next.
! Here, we prefer to move on to the next file
! rather than the next time step in the old file
! to use model arrays closer to reanalysis.
! If there is no overlap, this routine will
! increment from one file to the next as expected.
!
! This subroutine will return istep=ifile=0, when
! there are no more steps to process.
implicit none
type(ncfile),dimension(:),intent(in)::files
integer,intent(inout)::ifile,istep

integer::nfile,nstep
type(ESMF_Time)::time1,time2,time3
nfile=size(files)
if(ifile.lt.1)then
    ! for initialization only
    ifile=1
    istep=1
    return
endif
nstep=size(files(ifile)%times)
time1=files(ifile)%times(istep)

if(istep+1.le.size(files(ifile)%times))then
    time2=files(ifile)%times(istep+1)
else
    ifile=ifile+1
    istep=1
    goto 990
endif

if(ifile+1.le.nfile)then
    ! if there is another file after the current one
    time3=files(ifile+1)%times(1)
else
    ! if there is not another file pick next time step in current file
    istep=istep+1
    goto 990
endif

if(time2.ge.time3)then
    ! if next file has a time step closer to the current time
    ! or if there are no more time steps in the current file,
    ! use the next file
    ifile=ifile+1
    istep=1
else
    ! otherwise use the next step in the current file
    istep=istep+1
endif

 990 continue
if(ifile.gt.nfile.or.istep.gt.size(files(ifile)%times))then
    ! make sure the current time step is valid
    ! end if not
    istep=0
    ifile=0
endif
end subroutine get_next_timestep

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

integer :: numarg,ifile,ii,nfile,s,sn,sd,ierr
logical::restart
character(len=120) :: wrfoutfname
character(len=*),parameter :: outputfname='moisture.nc'
character(len=64)::timediffstr

type(ncfile),dimension(:),allocatable::wrffile
type(ncfile)::wrfoutfile
type(ncvars)::vars
type(ESMF_Time)::oldtime,timenow,starttime
type(ESMF_TimeInterval)::timedt

!integer,external :: iargc

! parse commandline argument for wrfout file name
numarg=iargc()

if(numarg .lt. 1)then
    print*, 'This program takes at least one argument, the wrfout files to run the moisture model from.'
    call abort()
endif

call getarg(1,wrfoutfname)
if(trim(wrfoutfname).eq.'-r')then
    allocate(wrffile(numarg-1))
else
    allocate(wrffile(numarg))
endif

nfile=0
restart=.false.
do i=1,numarg
    call getarg(i,wrfoutfname)
    if(trim(wrfoutfname).eq.'-r')then
        restart=.true.
        call initialize_and_check(outputfname,wrfoutfile)
    else
        print*,'using: '//trim(wrfoutfname)
        nfile=nfile+1
        call initialize_and_check(wrfoutfname,wrffile(nfile))
    endif
enddo
call initialize_vars(vars)
oldtime=wrffile(1)%times(1)
timenow=oldtime
nsteps=99999

if(.not.restart) &
call create_output(outputfname)

!*** executable
fire_print_msg = 2

call init_fuel_cats(.true.)
print *,moisture_classes,' moisture classes'

ifile=0
ii=0
istep=0
call get_next_timestep(wrffile,ifile,ii)
if(restart)then
    starttime=wrfoutfile%times(size(wrfoutfile%times))
else
    starttime=wrffile(1)%times(1)
endif
do while(restart.and.wrffile(ifile)%times(ii).le.starttime)
    call get_next_timestep(wrffile,ifile,ii)
    if(ifile.eq.0)goto 1000
enddo
 1000 continue
if(ifile.eq.0)then
    call crash('no new time steps available')
endif
print*,'starting at file ',ifile,' step ',ii
call ESMF_TimeGet(wrffile(ifile)%times(ii),TimeString=timediffstr)
print*,'wrffile time=',trim(timediffstr)
call ESMF_TimeGet(starttime,TimeString=timediffstr)
print*,'restart time=',trim(timediffstr)
call get_next_timestep(wrffile,ifile,ii)

do while(ifile.gt.0)
    istep=istep+1
    !endif

    if(restart)then
        call read_file(wrfoutfile,-1,vars,ierr)
        if(ierr.ne.0)then
            call crash('invalid restart file')
        endif
        fmoist_init=3
    else
        call read_file(wrffile(ifile),ii,vars,ierr)
        if(ierr.ne.0)then
            print*,'invalid data: skipping file ',ifile,' step ',ii
            goto 10001
        endif
    endif
   
    oldtime=timenow
    timenow=wrffile(ifile)%times(ii)
    print*,'reading file ',ifile,' of ',nfile
    print*,'time step ',ii,' of ',wrffile(ifile)%ntime
    !if(istep.gt.1)then
    call ESMF_TimeGet(timenow,timeString=timediffstr)
    print*,'time: ',trim(timediffstr)

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
    if(restart)then
        restart=.false.
    else
        call get_next_timestep(wrffile,ifile,ii)
    endif
    10001 continue
enddo
 99 continue
call destroy_vars(vars)
do i=1,nfile
    call destroy_file(wrffile(i))
enddo
call alldone()
end program moisture_main
