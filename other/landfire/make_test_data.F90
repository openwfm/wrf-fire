program convert_landfire
implicit none

character(len=256)::buffer,fname,oname
integer::ncol,nrow,ierr,i,j,iread,iwrite,k,ictile,irtile,ctile,rtile,l,sxtile,extile,sytile,eytile
integer,parameter::maxtile=1000, maxcat=14
real(kind=4)::rbuf
integer(kind=2)::ibuf

call getarg(1,fname)
call getarg(2,buffer)
read(buffer,*) nrow
call getarg(3,buffer)
read(buffer,*) ncol

iread=60
iwrite=61
!open(iread,file=fname,status='old',form='unformatted',access='direct',recl=4)

call get_ntile(ctile,maxtile,ncol)
call get_ntile(rtile,maxtile,nrow)
print*,'splitting data into ',rtile,'x',ctile,' tiles'
k=0
do ictile=1,ctile
  do irtile=1,rtile

    ibuf=mod(k,maxcat)+1

    k=k+1
    call get_tile_idx(sxtile,extile,ictile,maxtile,ncol)
    call get_tile_idx(sytile,eytile,irtile,maxtile,nrow)
    call get_tile_name(oname,sytile,eytile,sxtile,extile)
    print*,'writing out file: ', trim(oname)
    open(iwrite,file=oname,status='unknown',form='unformatted',access='direct',recl=2)
    l=0
    do i=sytile,eytile
      do j=sxtile,extile
         l=l+1
!         read(iread,rec=k,err=999) rbuf
!         if(rbuf.gt.maxcat.or.rbuf.lt.1)rbuf=0.
         write(iwrite,rec=l) ibuf
       enddo
     enddo
     close(iwrite)
   enddo
enddo

999 continue
print*,i,j,k,l,irtile,ictile
!close(iread)
end program convert_landfire

subroutine get_ntile(ntile,maxtile,n)
implicit none
integer,intent(out)::ntile
integer,intent(in)::maxtile,n
ntile=(n-1)/maxtile+1
end subroutine get_ntile

subroutine get_tile_idx(stile,etile,itile,maxtile,n)
implicit none
integer,intent(out)::stile,etile
integer,intent(in)::itile,maxtile,n
stile=(itile-1)*maxtile+1
etile=min(itile*maxtile,n)
end subroutine get_tile_idx

subroutine get_tile_name(tname,sxtile,extile,sytile,eytile)
implicit none
character(len=*),intent(out)::tname
integer,intent(in)::sxtile,extile,sytile,eytile

write(tname,'(i5.5,"-",i5.5,".",i5.5,"-",i5.5)') sytile,eytile,sxtile,extile
end subroutine get_tile_name
