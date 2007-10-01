
program netcdf_crop
  use netcdf
  implicit none

  integer n,i,j
  character (len=256) :: carg,fin,fout
  character (len=nf90_max_name) nname,tname,aname
  integer tval,ncidin,ncidout,ndims,nvars,ngattr,nvattr,nunlim,nvdims
  integer ilen,itmp,nlen,tid
  integer,dimension(nf90_max_var_dims) :: dimids,odimids
  integer,dimension(nf90_max_dims) :: dlens,dconv
  integer,dimension(nf90_max_vars) :: varids,xtype
  integer,dimension(:),allocatable :: ivals
  character,dimension(:),allocatable :: cvals
  real(kind=kind(0.0d0)),dimension(:),allocatable :: fvals
  integer,dimension(:),allocatable :: start,count,ostart,ocount

  n=iargc()
  if(n.ne.4)then
    print*,'usage: netcdf_crop infilename outfilename timename time'
    print*,'infilename: netcdf file to import'
    print*,'outfilename: netcdf file to export'
    print*,'timename: name of time dimension in netcdf file'
    print*,'time: time to export'
    goto 999
  endif
  call getarg(1,fin)
  call getarg(2,fout)
  call getarg(4,carg)
  call getarg(3,tname)
  read(carg,*)tval

  call check(nf90_open(fin,nf90_nowrite,ncidin))
  call check(nf90_create(fout,nf90_clobber,ncidout))
  call check(nf90_inquire(ncidin,nDimensions=ndims, &
                                 nVariables=nvars, &
                                 nAttributes=ngattr, &
                                 unlimitedDimId=nunlim))
  call check(nf90_inq_dimid(ncidin,tname,tid))
  do i=1,ndims
    call check(nf90_inquire_dimension(ncidin,i,name=nname,len=dlens(i)))
    if(i.ne.tid)then
      call check(nf90_def_dim(ncidout,nname,dlens(i),dconv(i)))
    else
      call check(nf90_def_dim(ncidout,nname,nf90_unlimited,dconv(i)))
    endif
  enddo
  do i=1,ngattr
    call check(nf90_inq_attname(ncidin,nf90_global,i,nname))
!    call check(nf90_inquire_attribute(ncidin,nf90_global,nname))
    call check(nf90_copy_att(ncidin,nf90_global,nname,ncidout,nf90_global))
  enddo
  do i=1,nvars
    call check(nf90_inquire_variable(ncidin,i,name=nname,&
                                              xtype=xtype(i),&
                                              ndims=nvdims,&
                                              dimids=dimids,&
                                              nAtts=nvattr))
    call convert_dimids(dconv,nvdims,dimids,odimids)
    call check(nf90_def_var(ncidout,nname,xtype(i),odimids(1:nvdims),varids(i)))
    do j=1,nvattr
      call check(nf90_inq_attname(ncidin,i,j,aname))
      call check(nf90_copy_att(ncidin,i,aname,ncidout,varids(i)))
    enddo
  enddo
  call check(nf90_enddef(ncidout))
  do i=1,nvars
    call check(nf90_inquire_variable(ncidin,i,dimids=dimids,ndims=nvdims))
    allocate(start(1:nvdims),count(1:nvdims),ostart(1:nvdims),ocount(1:nvdims))
    do j=1,nvdims
      if(dimids(j).ne.tid)then
        start(j)=1
        count(j)=dlens(dimids(j))
        ostart(j)=1
        ocount(j)=dlens(dimids(j))
      else
        start(j)=tval
        count(j)=1
        ostart(j)=1
        ocount(j)=1
      endif
    enddo
    call get_nlen(dlens,nvdims,dimids,tid,nlen)
    if(xtype(i).eq.nf90_char)then
      allocate(cvals(1:nlen))
      call check(nf90_get_var(ncidin,i,cvals,start=start,count=count))
      call check(nf90_put_var(ncidout,varids(i),cvals,start=ostart,&
                                                      count=ocount))
      deallocate(cvals)
    elseif(xtype(i).eq.nf90_byte.or.&
           xtype(i).eq.nf90_short.or.&
           xtype(i).eq.nf90_int)then
      allocate(ivals(1:nlen))
      call check(nf90_get_var(ncidin,i,ivals,start=start,count=count))
      call check(nf90_put_var(ncidout,varids(i),ivals,start=ostart,&
                                                      count=ocount))
      deallocate(ivals)
    elseif(xtype(i).eq.nf90_float.or.&
           xtype(i).eq.nf90_double)then
      allocate(fvals(1:nlen))
      call check(nf90_get_var(ncidin,i,fvals,start=start,count=count))
      call check(nf90_put_var(ncidout,varids(i),fvals,start=ostart,&
                                                      count=ocount))
      deallocate(fvals)
    else
      call check(nf90_ebadtype)
    endif
    deallocate(start,count,ostart,ocount)
  enddo
 
  call check(nf90_close(ncidin))
  call check(nf90_close(ncidout))
  999 continue
contains
  subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
      print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if
  end subroutine check  

  subroutine convert_dimids(dconv,nd,din,dout)
  use netcdf
  implicit none
  integer,dimension(nf90_max_dims),intent(in) :: dconv
  integer,intent(in)::nd
  integer,dimension(nd),intent(in)::din
  integer,dimension(nd),intent(out)::dout
  integer i

  do i=1,nd
    dout(i)=dconv(din(i))
  enddo 

  end subroutine convert_dimids

  subroutine get_nlen(dlens,nd,dimids,tid,nlen)
  use netcdf
  implicit none
  integer,dimension(nf90_max_dims),intent(in)::dlens
  integer,intent(in)::nd
  integer,dimension(nd),intent(in)::dimids
  integer,intent(in)::tid
  integer,intent(out)::nlen
  integer::i
  nlen=1
  do i=1,nd
    if(dimids(i).ne.tid)then
      nlen=nlen*dlens(dimids(i))
    endif
  enddo
  end subroutine get_nlen

end program netcdf_crop

