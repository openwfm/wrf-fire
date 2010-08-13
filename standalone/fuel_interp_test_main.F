	program test
	use module_fr_sfire_core
	! add call to your subroutine here 
        implicit none
real :: time_now
integer :: ims,ime,jms,jme,icl,jcl,i,j
real,dimension(3,3)::tff,lff,lfn,tign,lfn1,tign1
real,dimension(3,3)::real_tff,real_lff
real, dimension(5,5)::d
real::tign_err
real::test1_err,test2_err,test3_err,test4_err,test5_err
real::lfn00,lfn01,lfn10,lfn11,tign00,tign01,tign10,tign11,fuel_time_cell
real:: Result1,Result2,Result3,Result4,Result5,result
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


fuel_time_cell= 8.235294 ;
        write(*,*)"Calculation of fuel_frac over 4 subcells"
result1=0;
do i=1,2
   do j=1,2

write(*,*)'lff',lff(i,j),lff(i,j+1),lff(i+1,j),lff(i+1,j+1)
result= result+fuel_left_cell_3(  &
    lff(i,j),lff(i,j+1),lff(i+1,j),lff(i+1,j+1), &
    tff(i,j),tff(i,j+1),tff(i+1,j),tff(i+1,j+1),&
    time_now, fuel_time_cell)
write(*,*)'result after step i and j',result,i,j
end do
end do


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

print*,""
print*,""
write(*,*)"Test1, result in Matlab u=(-2;0;1) f =0.9169"

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
print*,""

write(*,*)"lfn",lfn00,lfn01,lfn10,lfn11
print*,""
write(*,*)"tign",tign00,tign01,tign10,tign11
print*,""
write(*,*)"Result1=",result1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

print*,""
print*,""
write(*,*)"Test2, result in Matlab u=(1;0;1) f =0.1660"

lfn00=-2.
lfn01=-2.
lfn10=-1.
lfn11=-1.
tign00=1.
tign01=1.
tign10=2.
tign11=2.
time_now=3
fuel_time_cell= 8.235294 ;

Result2= fuel_left_cell_3(  &
    lfn00,lfn01,lfn10,lfn11, &
    tign00,tign01,tign10,tign11,&
    time_now, fuel_time_cell)
print*,""

write(*,*)"lfn",lfn00,lfn01,lfn10,lfn11
print*,""
write(*,*)"tign",tign00,tign01,tign10,tign11
print*,""

write(*,*)"Result2=",Result2

print*,""
print*,""
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
write(*,*)"Test3, result in Matlab u=-(2;0;4) f =0.4538"

lfn00=-2.
lfn01=-2.
lfn10=-4.
lfn11=-4.
tign00=6.
tign01=6.
tign10=4.
tign11=4.
time_now=10
fuel_time_cell= 8.235294 ;

result3= fuel_left_cell_3(  &
    lfn00,lfn01,lfn10,lfn11, &
    tign00,tign01,tign10,tign11,&
    time_now, fuel_time_cell)
print*,""

write(*,*)"lfn",lfn00,lfn01,lfn10,lfn11
print*,""
write(*,*)"tign",tign00,tign01,tign10,tign11
print*,""

write(*,*)"Result3=",result3

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

print*,""
print*,""
write(*,*)"Test4, result in Matlab u=(-4;-4;2) f =0.7891"

lfn00=-3.
lfn01=-1.
lfn10=-1.
lfn11=1.
tign00=4.
tign01=8.
tign10=8.
tign11=10.
time_now=10
fuel_time_cell= 8.235294 ;

Result4= fuel_left_cell_3(  &
    lfn00,lfn01,lfn10,lfn11, &
    tign00,tign01,tign10,tign11,&
    time_now, fuel_time_cell)
print*,""

write(*,*)"lfn",lfn00,lfn01,lfn10,lfn11
print*,""
write(*,*)"tign",tign00,tign01,tign10,tign11
print*,""

write(*,*)"Result4=",Result4

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

print*,""
print*,""
write(*,*)"Test5, result in Matlab u=(-2;-2;3) f =0.9488"

lfn00=-1.
lfn01=1.
lfn10=1.
lfn11=3.
tign00=1.
tign01=2.
tign10=2.
tign11=2.
time_now=2
fuel_time_cell= 8.235294 ;

Result5= fuel_left_cell_3(  &
    lfn00,lfn01,lfn10,lfn11, &
    tign00,tign01,tign10,tign11,&
    time_now, fuel_time_cell)
print*,""

write(*,*)"lfn",lfn00,lfn01,lfn10,lfn11
print*,""
write(*,*)"tign",tign00,tign01,tign10,tign11
print*,""

write(*,*)"Result5=",Result5

print*,""

print*,""
print*,""
print*,""
print*,""
print*,""



write(*,101)'tign',tign
write(*,101)'lfn',lfn
print*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
print*,"Big test, lfn- plane z=y-2; tign=y tnow=2"


time_now=2.

!data lfn1/2.0, 1.0, 0.0, &
!         1.0, 0.0,-1.0, &
!         0.0, -1.0,-2.0/
!data tign1/3.0, 3.0, 3.0, &
!          3.0, 3.0, 2.0, &
!          3.0, 2.0, 1.0 /
data lfn1/-1., 0., 1., &
         -1., 0.,1., &
         -1., 0.,1./

data tign1/1.0, 2., 2., &
          1.0, 2., 2., &
          1.0, 2., 2. /

print*,time_now
write(*,101)'tign',tign1
write(*,101)'lfn',lfn1
!data real_tff/3.0, 2.5, 2.0, &
!          3.0, 2.5, 2.0, &
!          3.0, 2.5, 2.0 /

!data real_lff/0.0, -0.5, -1.0, &
!          0.0, -0.5, -1.0, &
!          0.0, -0.5, -1.0 /

        print *,"this is fuel_interp_test.exe"
        call tign_lfn_interpolation(time_now,icl,jcl,ims,ime,jms,jme, &
                                    tign1,lfn1,tff,lff)
write(*,101)'tign',tign1
write(*,101)'lfn',lfn1
write(*,101)'tff',tff
write(*,101)'lff',lff

d=1e20
d(1:5:2,1:5:2)=tign1
d(2:4,2:4)=tff
write(*,*)'tign and tff'
write(*,'(5f7.3)')d
write(*,*)' '
d(1:5:2,1:5:2)=lfn1
d(2:4,2:4)=lff
write(*,*)'lfn and lff'
write(*,'(5f7.3)')d
write(*,*)' '

 !       write(*,*)"Real result - calculated result"
 !	write(*,*)real_tff-tff
 !	test1_err=maxval(abs(real_tff-tff))
 !	write(*,*)"test1_err=",test1_err


fuel_time_cell= 8.235294 ;
        write(*,*)"Calculation of fuel_frac over 4 subcells"
result1=0;
do i=1,2
   do j=1,2

write(*,*)'lff',lff(i,j),lff(i,j+1),lff(i+1,j),lff(i+1,j+1)
result= result+fuel_left_cell_3(  &
    lff(i,j),lff(i,j+1),lff(i+1,j),lff(i+1,j+1), &
    tff(i,j),tff(i,j+1),tff(i+1,j),tff(i+1,j+1),&
    time_now, fuel_time_cell)
write(*,*)'result after step i and j',result,i,j
end do
end do


!!!! Test #2







 end program test
