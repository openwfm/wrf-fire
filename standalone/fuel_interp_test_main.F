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
real::test1_err,test2_err,test3_err,test4_err,test5_err
real::lfn00,lfn01,lfn10,lfn11,tign00,tign01,tign10,tign11,fuel_time_cell
real:: Result1
!data lfn/2., 1., 0., &
!         1., 0.,-1., &
!         0.,-1.,-2./
time_now=3.
ims=1
ime=3
jms=1
jme=3
icl=2
jcl=2


!<<<<<<< HEAD:standalone/fuel_interp_test_main.F

!!!!! Test #1


!lfn - plane Z=1.5-X;
! tign -- everywhere where lfn>=0, tign=3;
! everywhere where lfn<0, tign=Z+3;
data lfn/0.5, -0.5, -1.5, &
         0.5, -0.5,-1.5, &
         0.5, -0.5,-1.5/
!>>>>>>> origin/jm2/vktest:standalone/fuel_interp_test_main.F

data tign/3.0, 2.5, 1.5, &
          3.0, 2.5, 1.5, &
          3.0, 2.5, 1.5 /

data real_tff/3.0, 2.5, 2.0, &
          3.0, 2.5, 2.0, &
          3.0, 2.5, 2.0 /

data real_lff/0.0, -0.5, -1.0, &
          0.0, -0.5, -1.0, &
          0.0, -0.5, -1.0 /

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
	test1_err=maxval(abs(real_tff-tff))
	write(*,*)"test1_err=",test1_err


!!!! Test #2

!=======
!lfn - plane Z=4-X-Y;
! tign -- everywhere where lfn>=0, tign=3;
! everywhere where lfn<0, tign=Z+3;
data lfn/2.0, 1.0, 0.0, &
         1.0, 0.0,-1.0, &
         0.0, -1.0,-2.0/

data tign/3.0, 3.0, 3.0, &
          3.0, 3.0, 2.0, &
          3.0, 2.0, 1.0 /

data real_tff/3.0, 3.0, 3.0, &
          3.0, 3.0, 2.5, &
          3.0, 2.5, 2.0 /

data real_lff/1.0, 0.5, 0.0, &
          0.5, 0.0, -0.5, &
          0.0, -0.5, -1.0 /

        print *,"this is fuel_interp_test.exe"
        call tign_lfn_interpolation(time_now,icl,jcl,ims,ime,jms,jme, &
                                    tign,lfn,tff,lff)
write(*,101)'tign',tign
write(*,101)'lfn',lfn
write(*,101)'tff',tign
write(*,101)'lff',lff

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
	test2_err=maxval(abs(real_tff-tff))
	write(*,*)"test2_err=",test2_err

!!!! Test #3

!=======
!lfn - plane Z=3-X-Y;
! tign -- everywhere where lfn>=0, tign=3;
! everywhere where lfn<0, tign=Z+3;
data lfn/1.0, 0.0, -1.0, &
         0.0, -1.0,-2.0, &
         -1.0, -2.0,-3.0/

data tign/3.0, 3.0, 2.0, &
          3.0, 2.0, 1.0, &
          2.0, 1.0, 0.0 /

data real_tff/3.0, 2.5, 2.0, &
          2.5, 2.0, 1.5, &
          2.0, 1.5, 1.0 /

data real_lff/0.0, -0.5, -1.0, &
          -0.5, -1.0, -1.5, &
          -1.0, -1.5, -2.0 /



print *,"this is fuel_interp_test.exe"
        call tign_lfn_interpolation(time_now,icl,jcl,ims,ime,jms,jme, &
                                    tign,lfn,tff,lff)
write(*,101)'tign',tign
write(*,101)'lfn',lfn
write(*,101)'tff',tign
write(*,101)'lff',lff

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
test3_err=maxval(abs(real_tff-tff))
        write(*,*)"test3_err=",test3_err

       

!!!! Test #4

!=======
!lfn - plane Z=5-X-Y;
! tign -- everywhere where lfn>=0, tign=3;
! everywhere where lfn<0, tign=Z+3;
data lfn/3.0, 2.0, 1.0, &
         2.0, 1.0, 0.0, &
         1.0, 0.0,-1.0/

data tign/3.0, 3.0, 3.0, &
          3.0, 3.0, 3.0, &
          3.0, 3.0, 2.0 /

data real_tff/3.0, 3.0, 3.0, &
          3.0, 3.0, 3.0, &
          3.0, 3.0, 3.0 /

data real_lff/2.0, 1.5, 1.0, &
          1.5, 1.0, 0.5, &
          1.0, 0.5, 0.0 /



print *,"this is fuel_interp_test.exe"
        call tign_lfn_interpolation(time_now,icl,jcl,ims,ime,jms,jme, &
                                    tign,lfn,tff,lff)
write(*,101)'tign',tign
write(*,101)'lfn',lfn
write(*,101)'tff',tign
write(*,101)'lff',lff

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
        test4_err=maxval(abs(real_tff-tff))
        write(*,*)"test4_err=",test4_err


        write(*,*)" "

        write(*,*)"test1_err=",test1_err
        write(*,*)"test2_err=",test2_err
        write(*,*)"test3_err=",test3_err
        write(*,*)"test4_err=",test4_err
        write(*,*)"If all test*_err are equal to 0.0, than the code works &
                    correctly for this set of tests"

        write(*,*)"Tests for fuel_left calculation"

lfn00=-1.
lfn01=-1.
lfn10=1.
lfn11=1.
tign00=1.
tign01=1.
tign10=2.
tign11=2.
time_now=2
fuel_time_cell= 8.235294 ;

result1= fuel_left_cell_3(  &
    lfn00,lfn01,lfn10,lfn11, &
    tign00,tign01,tign10,tign11,&
    time_now, fuel_time_cell)

write(*,*)"Result1=",result1

 end program test
