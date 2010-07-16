	program test
	use module_fr_sfire_core
	! add call to your subroutine here 
        implicit none
real :: time_now
integer :: ims,ime,jms,jme,icl,jcl
real,dimension(:,:),allocatable :: lfn,tign
real,dimension(3,3)::tff,lff
real,dimension(3,3)::real_tff,real_lff
time_now=3.
!lfn - plane Z=1.5-X;
! tign -- everywhere where lfn>=0, tign=3;
! everywhere where lfn<0, tign=Z+3;
ims=1
ime=3
jms=1
jme=3
   allocate(lfn(ims:ime,jms:jme))
   allocate(tign(ims:ime,jms:jme))
icl=2
jcl=2
lfn(1,1)=0.5
lfn(1,2)=0.5
lfn(1,3)=0.5

lfn(2,1)=-0.5
lfn(2,2)=-0.5
lfn(2,3)=-0.5

lfn(3,1)=-1.5
lfn(3,2)=-1.5
lfn(3,3)=-1.5

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
100 format (3(A,F7.3))
        print *,"table of tign of cells and subcells"
        write(*,100) "     cell(3,1)  ",tign(3,1),"               cell(3,2)  ",tign(3,2),"               cell(3,3)  ",tign(3,3)
        write(*,*)" "
        write(*,100) "                 tff(3,1)   ",tff(3,1),"   tff(3,2)   ",tff(3,2),"     tff(3,3)   ",tff(3,3)             
        write(*,*)" "
        write(*,100) "     cell(2,1)  ",tign(2,1),"               cell(2,2)  ",tign(2,2),"               cell(2,3)  ",tign(2,3)
        write(*,*)"                         /             |                   \  "
        write(*,100) "                 tff(2,1)   ",tff(2,1),"   tff(2,2)   ",tff(2,2),"     tff(2,3)   ",tff(2,3)             
        write(*,*)" "
        write(*,100) "                 tff(1,1)   ",tff(1,1),"   tff(1,2)   ",tff(1,2),"     tff(1,3)   ",tff(1,3)             
        write(*,*)" "
        write(*,100) "     cell(1,1)  ",tign(1,1),"               cell(1,2)  ",tign(1,2),"               cell(1,3)  ",tign(1,3)
        write(*,*)" "
        write(*,*)" "
        write(*,*)" "
        print *,"table of lfn of cells and subcells"
        write(*,100) "     cell(3,1)  ",lfn(3,1),"               cell(3,2)  ",lfn(3,2),"               cell(3,3)  ",lfn(3,3)
        write(*,*)" "
        write(*,100) "                 lff(3,1)   ",lff(3,1),"   lff(3,2)   ",lff(3,2),"     lff(3,3)   ",lff(3,3)             
        write(*,*)" "
        write(*,100) "     cell(2,1)  ",lfn(2,1),"               cell(2,2)  ",lfn(2,2),"               cell(2,3)  ",lfn(2,3)
        write(*,*)"                         /             |                   \  "
        write(*,100) "                 lff(2,1)   ",lff(2,1),"   lff(2,2)   ",lff(2,2),"     lff(2,3)   ",lff(2,3)             
        write(*,*)" "
        write(*,100) "                 lff(1,1)   ",lff(1,1),"   lff(1,2)   ",lff(1,2),"     lff(1,3)   ",lff(1,3)             
        write(*,*)" "
        write(*,100) "     cell(1,1)  ",lfn(1,1),"               cell(1,2)  ",lfn(1,2),"               cell(1,3)  ",lfn(1,3)

        write(*,*)" "
        write(*,*)" "
        write(*,*)" "
       
        write(*,*)"Real result - calculated result"
       
110 format (A,3(F7.3))
        write(*,110) "real_tff-tff (3.1-3.3)",tff(3,1)-real_tff(3,1),tff(3,2)-real_tff(3,2),tff(3,3)-real_tff(3,3)
        write(*,110) "real_tff-tff (2.1-2.3)",tff(2,1)-real_tff(2,1),tff(2,2)-real_tff(2,2),tff(2,3)-real_tff(2,3)
        write(*,110) "real_tff-tff (1.1-1.3)",tff(1,1)-real_tff(1,1),tff(1,2)-real_tff(1,2),tff(1,3)-real_tff(1,3)
        write(*,110) "real_lff-lff (3.1-3.3)",lff(3,1)-real_lff(3,1),lff(3,2)-real_lff(3,2),lff(3,3)-real_lff(3,3)
        write(*,110) "real_lff-lff (2.1-2.3)",lff(2,1)-real_lff(2,1),lff(2,2)-real_lff(2,2),lff(2,3)-real_lff(2,3)
        write(*,110) "real_lff-lff (1.1-1.3)",lff(1,1)-real_lff(1,1),lff(1,2)-real_lff(1,2),lff(1,3)-real_lff(1,3)

	deallocate(tign)
	deallocate(lfn)
        end program test
