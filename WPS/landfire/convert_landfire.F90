program convert_landfire
implicit none

character(len=256)::buffer,fname,oname
integer::ncol,nrow,ierr,i,j,iread,iwrite,k,itile,jtile,ntile,mtile,l,sxtile,extile,sytile,eytile
integer,parameter::maxtile=1000, maxcat=14
real(kind=4)::rbuf

call getarg(1,fname)
call getarg(2,buffer)
read(buffer,*) nrow
call getarg(3,buffer)
read(buffer,*) ncol

iread=60
iwrite=61
open(iread,file=fname,status='old',form='unformatted',access='direct',recl=1)

call get_ntile(ntile,maxtile,ncol)
call get_ntile(mtile,maxtile,nrow)
print*,'splitting data into ',mtile,'x',ntile,' tiles'
k=0
do jtile=1,ntile
  do itile=1,mtile
    call get_tile_idx(sxtile,extile,itile,maxtile,nrow)
    call get_tile_idx(sytile,eytile,jtile,maxtile,ncol)
    call get_tile_name(oname,sxtile,extile,sytile,eytile)
    print*,'writing out file: ', trim(oname)
    open(iwrite,file=oname,status='unknown',form='unformatted',access='direct',recl=1)
    l=0
    do j=sytile,eytile
      do i=sxtile,extile
         k=(j-1)*nrow+i
         l=l+1
         read(iread,rec=k,err=999) rbuf
         if(rbuf.gt.maxcat.or.rbuf.lt.1)rbuf=0.
         write(iwrite,rec=l) int(rbuf,kind=4)
       enddo
     enddo
     close(iwrite)
   enddo
enddo

999 continue
print*,i,j,k,l,itile,jtile
close(iread)
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

write(tname,'(i5.5,",",i5.5,".",i5.5,",",i5.5)') sytile,eytile,sxtile,extile
end subroutine get_tile_name
