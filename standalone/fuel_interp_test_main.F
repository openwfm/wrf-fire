	program test
	use module_fr_sfire_core
	! add call to your subroutine here 
        implicit none
real :: time_now
integer :: ims,ime,jms,jme,icl,jcl
real,dimension(:,:),allocatable :: lfn,tign
real,dimension(3,3)::tff,lff
time_now=3.
!lfn - plane Z=4-X-Y;
! tign -- everywhere where lfn>=0, tign=3;
! everywhere where lfn<0, tig:n=Z+3;
ims=1
ime=3
jms=1
jme=3
   allocate(lfn(ims:ime,jms:jme))
   allocate(tign(ims:ime,jms:jme))
icl=2
jcl=2
lfn(1,1)=2.
lfn(1,2)=1.
lfn(1,3)=0.

lfn(2,1)=1.
lfn(2,2)=0.
lfn(2,3)=-1.

lfn(3,1)=0.
lfn(3,2)=-1.
lfn(3,3)=-2.

tign(1,1)=3.
tign(1,2)=3.
tign(1,3)=3.

tign(2,1)=3.
tign(2,2)=3.
tign(2,3)=2.

tign(3,1)=3.
tign(3,2)=2.
tign(3,3)=1.
        print *,"Hello"
        print *,"this is fuel_interp_test.exe"
call tign_lfn_interpolation(time_now,icl,jcl,ims,ime,jms,jme, &
                                  tign,lfn,tff,lff)
        print *,"tff",tff
        print *,"lff",lff
	print *,"I am not doing anything yet"
	deallocate(tign)
	deallocate(lfn)
        end program test
