module saved_stats
use netcdf
implicit none
real(kind=8),save::smax,smean,ssq
character(len=nf90_max_name),save::vmax,vmean,vssq
private::smax,smean,ssq,vmax,vmean,vssq
contains

subroutine reset_stats
implicit none
smax=-1
smean=-1
ssq=-1
vssq=" "
vmax=" "
vmean=" "
end subroutine reset_stats

subroutine set_stats(vname,infnorm,mean,meansq)
implicit none
character(len=*),intent(in)::vname
real(kind=8),intent(in)::infnorm,mean,meansq
if(infnorm.gt.smax)then
  smax=infnorm
  vmax=vname
endif
if(mean.gt.smean)then
  smean=mean
  vmean=vname
endif
if(meansq.gt.ssq)then
  ssq=meansq
  vssq=vname
endif
end subroutine set_stats

subroutine print_stats
implicit none
if(smax.gt.0d0)then
  print*,"maximum error=",smax," in variable ",trim(vmax)
endif
if(smean.gt.0d0)then
  print*,"maximum mean error=",smean," in variable ",trim(vmean)
endif
if(ssq.gt.0d0)then
  print*,"maximum mean square error=",ssq," in variable ",trim(vssq)
endif
if(smax.le.0d0.and.smean.le.0d0.and.ssq.le.0d0)then
  print*,"No errors found in this time slice"
endif
end subroutine print_stats

end module saved_stats


program netcdf_diff
use netcdf
use saved_stats
implicit none
integer::iargc,narg,i
character(len=256)::carg,f1,f2,outfile
integer::nvars,ierr,t1,t2,nt1,nt2,nc1,nc2
integer,allocatable,dimension(:)::varids

narg=iargc()
if(narg.ne.2)goto 999

call getarg(1,f1)
call getarg(2,f2)
!call getarg(3,outfile)

call check(nf90_open(f1,nf90_nowrite,nc1))
call check(nf90_open(f2,nf90_nowrite,nc2))
call check(nf90_inquire(nc1,unlimiteddimid=t1))
call check(nf90_inquire(nc2,unlimiteddimid=t2))
nt1=1
nt2=1
if(t1.ne.-1)then
  call check(nf90_inquire_dimension(nc1,t1,len=nt1))
endif
if(t2.ne.-1)then
  call check(nf90_inquire_dimension(nc2,t2,len=nt2))
endif
if(nt1.ne.nt2)then
  print*,"WARNING: input netcdf files do not match, different number of time slices"
endif
do i=1,min(nt1,nt2)
  call reset_stats
  if(t1.ne.-1.and.t2.ne.-1)then
    print*," "
    print*,"Time slice #",i," of ",min(nt1,nt2)
    print*," "
  endif
  call compare_time_slice(nc1,nc2,i,t1,t2)
  print*," "
  call print_stats
  print*," "
enddo
goto 10
 999 continue
print*,"usage: netcdf_diff infile1 infile2"
stop 1
 10 continue
end program netcdf_diff

subroutine compare_time_slice(nc1,nc2,i,t1,t2)
use netcdf
implicit none
integer,intent(in)::nc1,nc2,i,t1,t2
character(len=nf90_max_name)::vname
integer::vid1,vid2,nvars
call check(nf90_inquire(nc1,nvariables=nvars))
do vid1=1,nvars
  call check(nf90_inquire_variable(nc1,vid1,name=vname))
  if(nf90_inq_varid(nc2,vname,vid2).ne.nf90_noerr)then
    print*,'WARNING: file #1 contains variable, ',vname,', but file #2 does not'
  else
    call compare_var(nc1,nc2,i,t1,t2,vname,vid1,vid2)
  endif
enddo
end subroutine compare_time_slice

subroutine compare_var(nc1,nc2,i,t1,t2,vname,vid1,vid2)
use netcdf
implicit none
integer,intent(in)::nc1,nc2,i,t1,t2,vid1,vid2
character(len=*),intent(in)::vname
real(kind=8),dimension(:),allocatable::v1,v2
integer::ierr,x1,x2,nd1,nd2,ndof1,ndof2
integer,dimension(nf90_max_var_dims)::d1,d2,start1,start2,count1,count2
call check(nf90_inquire_variable(nc1,vid1,xtype=x1,ndims=nd1,dimids=d1))
call check(nf90_inquire_variable(nc2,vid2,xtype=x2,ndims=nd2,dimids=d2))
if(x1.ne.x2)then
  print*,"WARNING: variable,",vname,", differs in type between the two files"
endif
if(x1.eq.nf90_char.or.x2.eq.nf90_char.or.x1.eq.nf90_int)return
call get_dim_sizes(nc1,i,t1,nd1,d1,start1,count1,ndof1)
call get_dim_sizes(nc2,i,t2,nd2,d2,start2,count2,ndof2)
if(ndof1.le.0.or.ndof2.le.0)then
  print*,"internal error, compare_var"
  call abort("crash")
endif
if(ndof1.ne.ndof2)then
  print*,"WARNING: ",trim(vname)," in file #1 has dofs: ",ndof1
  print*,"               and in file #2 has dofs: ",ndof2
endif
allocate(v1(ndof1),stat=ierr)
if(ierr.ne.0)then 
  call abort("allocation error")
endif
allocate(v2(ndof2),stat=ierr)
if(ierr.ne.0)then
  call abort("allocation error")
endif
call check(nf90_get_var(nc1,vid1,v1,start=start1,count=count1))
call check(nf90_get_var(nc2,vid2,v2,start=start2,count=count2))
call compare_data(vname,min(ndof1,ndof2),v1,v2)
deallocate(v1,stat=ierr)
if(ierr.ne.0)then
  call abort("deallocation error")
endif
deallocate(v2,stat=ierr)
if(ierr.ne.0)then
  call abort("deallocation error")
endif

end subroutine compare_var

subroutine compare_data(vname,dof,v1,v2)
use saved_stats
implicit none
character(len=*),intent(in)::vname
integer,intent(in)::dof
real(kind=8),dimension(dof),intent(in)::v1,v2
real(kind=8)::mean,amax
external::mean,amax
real(kind=8),dimension(dof)::diff
real(kind=8)::vmean,vssq,vmax

diff=abs(v1-v2)
vmean=mean(dof,diff)
vmax=amax(dof,diff)
diff=diff*diff
vssq=mean(dof,diff)
call set_stats(vname,vmax,vmean,vssq)

if(vmax.le.0d0.and.vmean.le.0d0.and.vssq.le.0d0)return
write(*,10),trim(vname),vmax,vmean,vssq
10 FORMAT(A,": maximum error=",e10.3," mean absolute error=",e10.3," mean square error=",e10.3)
end subroutine compare_data

real(kind=8) function amax(n,a)
implicit none
integer,intent(in)::n
real(kind=8),dimension(n),intent(in)::a
integer::i
amax=-1d0
do i=1,n
  amax=max(amax,a(i))
enddo
end function amax

real(kind=8) function mean(n,a)
implicit none
integer,intent(in)::n
real(kind=8),dimension(n),intent(in)::a
integer::i
mean=0
do i=1,n
  mean=mean+a(i)
enddo
mean=mean/n
end function mean

subroutine get_dim_sizes(nc,ti,t,nd,d,start,count,ndof)
use netcdf
implicit none
integer,intent(in)::nc,ti,t
integer,intent(in)::nd
integer,dimension(nf90_max_var_dims),intent(in)::d
integer,dimension(nf90_max_var_dims),intent(out)::start,count
integer,intent(out)::ndof
integer::i

ndof=1
do i=1,nd
  if(d(i).eq.t)then
    start(i)=ti
    count(i)=1
  else
    call check(nf90_inquire_dimension(nc,d(i),len=count(i)))
    start(i)=1
    ndof=ndof*count(i)
  endif
enddo
end subroutine get_dim_sizes

subroutine check(ncerr)
use netcdf
implicit none
integer,intent(in)::ncerr
if(ncerr.ne.nf90_noerr)then
  print*,nf90_strerror(ncerr)
else
  return
endif
call abort("error")
end subroutine check

