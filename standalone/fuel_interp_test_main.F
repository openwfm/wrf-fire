	program test
	use module_fr_sfire_core
	! add call to your subroutine here 
        implicit none
real :: time_now
integer :: ims,ime,jms,jme,icl,jcl
real,dimension(3,3)::tff,lff,lfn,tign
real,dimension(3,3)::real_tff,real_lff
real, dimension(5,5)::d
real::tign_err
data lfn/2., 1., 0., &
         1., 0.,-1., &
         0.,-1.,-2./
time_now=3.
!lfn - plane Z=1.5-X;
! tign -- everywhere where lfn>=0, tign=3;
! everywhere where lfn<0, tign=Z+3;
ims=1
ime=3
jms=1
jme=3
icl=2
jcl=2
<<<<<<< HEAD:standalone/fuel_interp_test_main.F
lfn(1,1)=0.5
lfn(1,2)=0.5
lfn(1,3)=0.5

lfn(2,1)=-0.5
lfn(2,2)=-0.5
lfn(2,3)=-0.5

lfn(3,1)=-1.5
lfn(3,2)=-1.5
lfn(3,3)=-1.5
=======
!lfn(1,1)=2.
!lfn(1,2)=1.
!lfn(1,3)=0.

!lfn(2,1)=1.
!lfn(2,2)=0.
!lfn(2,3)=-1.

!lfn(3,1)=0.
!lfn(3,2)=-1.
!lfn(3,3)=-2.
>>>>>>> origin/jm2/vktest:standalone/fuel_interp_test_main.F

tign(1,1)=3.
tign(1,2)=3.
tign(1,3)=3.

tign(2,1)=2.5
tign(2,2)=2.5
tign(2,3)=2.5

tign(3,1)=1.5
tign(3,2)=1.5
tign(3,3)=1.5

real_tff(1,1)=3.
real_tff(1,2)=3.
real_tff(1,3)=3.

real_tff(2,1)=2.5
real_tff(2,2)=2.5
real_tff(2,3)=2.5

real_tff(3,1)=2.
real_tff(3,2)=2.
real_tff(3,3)=2.

real_lff(1,1)=0.
real_lff(1,2)=0.
real_lff(1,3)=0.
    
real_lff(2,1)=-0.5
real_lff(2,2)=-0.5
real_lff(2,3)=-0.5

real_lff(3,1)=-1.
real_lff(3,2)=-1.
real_lff(3,3)=-1.

        print *,"this is fuel_interp_test.exe"
        call tign_lfn_interpolation(time_now,icl,jcl,ims,ime,jms,jme, &
                                    tign,lfn,tff,lff)
write(*,101)'tign',tign
write(*,101)'lfn',lfn
write(*,101)'tff',tign
write(*,101)'lff',lff
101 format(a/(3f7.3))

d=1e20
d(1:5:2,1:5:2)=tign
d(2:4,2:4)=tff
write(*,*)'tign and tff'
write(*,'(5f7.3)')d
write(*,*)' '
d(1:5:2,1:5:2)=lfn
d(2:4,2:4)=lff
write(*,*)'lfn and lff'
write(*,'(5f7.3)')d
write(*,*)' '

        write(*,*)"Real result - calculated result"
	write(*,*)real_tff-tff
	tign_err=maxval(abs(real_tff-tff))
	write(*,*)"tign_err=",tign_err

        end program test
