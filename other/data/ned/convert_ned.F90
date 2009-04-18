program convert_landfire
implicit none

character(len=256)::buffer,fname,oname
integer::ncol,nrow,ierr,i,j,iread,iwrite,k,ictile,irtile,ctile,rtile,l,sxtile,extile,sytile,eytile,grow,gcol
integer,parameter::maxtile=1000, maxcat=14,halo=3
real(kind=4)::rbuf

call getarg(1,fname)
call getarg(2,buffer)
read(buffer,*) grow
call getarg(3,buffer)
read(buffer,*) gcol


iread=60
iwrite=61
nrow=grow-2*halo
ncol=gcol-2*halo
open(iread,file=fname,status='old',form='unformatted',access='direct',recl=4)

call get_ntile(ctile,maxtile,ncol)  ! x
call get_ntile(rtile,maxtile,nrow)  ! y
print*,'splitting data into ',rtile,'x',ctile,' tiles'
k=0
do irtile=1,rtile ! y
  do ictile=1,ctile ! x
    call get_tile_idx(sxtile,extile,ictile,maxtile,ncol)
    call get_tile_idx(sytile,eytile,irtile,maxtile,nrow)
    call get_tile_name(oname,sxtile,extile,sytile,eytile)
    print*,'writing out file: ', trim(oname)
    open(iwrite,file=oname,status='unknown',form='unformatted',access='direct',recl=2)
    l=0
    do i=sytile,eytile+2*halo  ! y 
      do j=sxtile,extile+2*halo ! x
         k=(i-1)*gcol+j
         l=l+1
         read(iread,rec=k,err=999) rbuf
         if(rbuf.lt.1)rbuf=0
         write(iwrite,rec=l) int(rbuf,kind=2)
       enddo
     enddo
     close(iwrite)
   enddo
enddo

print*,'completed successfully'
999 continue
print*,i,j,k,l,irtile,ictile
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

write(tname,'(i5.5,"-",i5.5,".",i5.5,"-",i5.5)') sxtile,extile,sytile,eytile
end subroutine get_tile_name

