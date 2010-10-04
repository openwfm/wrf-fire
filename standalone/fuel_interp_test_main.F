	program test
	use module_fr_sfire_core
        implicit none
real :: time_now
integer :: ims,ime,jms,jme,icl,jcl,i,j
real,dimension(3,3)::tff,lff,lfn,tign,lfn1,tign1,lfn2,tign2
real,dimension(3,3)::real_tff,real_lff
real, dimension(5,5)::d
real, dimension(5,3)::lfn3,tign3
real::tign_err
real,dimension(2,2)::rslt
real::test1_err,test2_err,test3_err,test4_err,test5_err
real::lfn00,lfn01,lfn10,lfn11,tign00,tign01,tign10,tign11,fuel_time_cell
real:: Result1,Result2,Result3,Result4,Result5,result
real:: res1,res2,res3,res4,res5
time_now=3.
101 format(a/(3f7.3))
ims=1
ime=3
jms=1
jme=3
icl=2
jcl=2

!!!!! Test #1
!lfn - plane Z=1.5-X;
! tign -- everywhere where lfn>=0, tign=3;
! everywhere where lfn<0, tign=Z+3;
data lfn/0.5, -0.5, -1.5, &
         0.5, -0.5,-1.5, &
         0.5, -0.5,-1.5/

data tign/3.0, 2.5, 1.5, &
          3.0, 2.5, 1.5, &
          3.0, 2.5, 1.5 /

data real_tff/3.0, 2.5, 2.0, &
          3.0, 2.5, 2.0, &
          3.0, 2.5, 2.0 /

data real_lff/0.0, -0.5, -1.0, &
          0.0, -0.5, -1.0, &
          0.0, -0.5, -1.0 /

        call tign_lfn_interpolation(time_now,icl,jcl,ims,ime,jms,jme, &
                                    tign,lfn,tff,lff)

	test1_err=maxval(abs(real_tff-tff))

!!!! Test #2
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

        call tign_lfn_interpolation(time_now,icl,jcl,ims,ime,jms,jme, &
                                    tign,lfn,tff,lff)

	test2_err=maxval(abs(real_tff-tff))

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



        call tign_lfn_interpolation(time_now,icl,jcl,ims,ime,jms,jme, &
                                    tign,lfn,tff,lff)

test3_err=maxval(abs(real_tff-tff))

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



        call tign_lfn_interpolation(time_now,icl,jcl,ims,ime,jms,jme, &
                                    tign,lfn,tff,lff)
        test4_err=maxval(abs(real_tff-tff))

        write(*,*)"test1_err=",test1_err
        write(*,*)"test2_err=",test2_err
        write(*,*)"test3_err=",test3_err
        write(*,*)"test4_err=",test4_err
        write(*,*)"If all test*_err are equal to 0.0, than the code works"
        write(*,*)"correctly for this set of tests"


print*,''
print*,''
print*,''
        write(*,*)"Tests for fuel_left calculation"

!write(*,*)"Test1, result in Matlab u=(-2;0;1) f =0.9169"
res1=0.9169
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
!print*,""
test1_err=res1-result1
!write(*,*)"lfn",lfn00,lfn01,lfn10,lfn11
!print*,""
!write(*,*)"tign",tign00,tign01,tign10,tign11
!print*,""
!write(*,*)"Result1=",result1
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!print*,""
!write(*,*)"Test2, result in Matlab u=(1;0;1) f =0.1660"
res2=0.166
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
!print*,""
test2_err=res2-Result2
!print*,""


!write(*,*)"Test3, result in Matlab u=-(2;0;4) f =0.4538"
res3=0.4538
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
!print*,""
test3_err=res3-result3

!print*,""


!write(*,*)"Test4, result in Matlab u=(-4;-4;6) f =0.5336"    
! -4 -4 2  .7891"
res4=0.5336
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
!print*,""
test4_err=res4-result4

!print*,""

!write(*,*)"Test5, result in Matlab u=(-2;-2;1) f =0.9692"
res5=0.9692
!-2 -2 3 .9488

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
!print*,""
test5_err=res5-result5
!print*,""
!print*,""


        write(*,*)"test1_err=",test1_err
        write(*,*)"test2_err=",test2_err
        write(*,*)"test3_err=",test3_err
        write(*,*)"test4_err=",test4_err
        write(*,*)"test5_err=",test5_err
        write(*,*)"If all test*_err are close to 0.0, than the code works"
        write(*,*)"correctly for this set of tests"






print*,""
print*,""
print*,""

print*,"Main Test #1, we calculate the fuel_left of the center cell"
print*,"(we have 9 cells initially)"
print*,"The initial values are"
print*,"lfn is described by the plane z=y-2; tign is described by the plane z=y"


time_now=2.

data lfn1/-1., 0., 1., &
         -1., 0.,1., &
         -1., 0.,1./

data tign1/1.0, 2., 2., &
          1.0, 2., 2., &
          1.0, 2., 2. /

print*,"time_now",time_now
!print*,"tign",tign1
!print*,"lfn",lfn1
write(*,101)'tign',tign1
write(*,101)'lfn',lfn1
print*,"icl,jcl should be equal 2 here"
write(*,*)'icl,jcl',icl,jcl
print*,"calling tign_lfn_interpolation, that calculates"
        call tign_lfn_interpolation(time_now,icl,jcl,ims,ime,jms,jme, &
                                    tign1,lfn1,tff,lff)

print*,"as result tff,lff are"
write(*,101)'tff',tff
write(*,101)'lff',lff

d=1e20
d(1:5:2,1:5:2)=tign1
d(2:4,2:4)=tff
write(*,*)'tign and tff, here you can check whether they were  & 
           calculated properly'
write(*,'(5f7.3)')d
write(*,*)' '
d(1:5:2,1:5:2)=lfn1
d(2:4,2:4)=lff
write(*,*)'lfn and lff, here you can check whether they were calculated properly'
write(*,'(5f7.3)')d
write(*,*)' '


fuel_time_cell= 8.235294 ;
print*,"Calculation of fuel_frac over 4 subcells"

result=0; ! Here we sum up all 4 results
do i=1,2  ! Loop over 4 subcells
   do j=1,2
print*,""
print*,"meaning of tff and lff in the subcell",i,j
write(*,*)'lff',lff(i,j),lff(i,j+1),lff(i+1,j),lff(i+1,j+1)
write(*,*)'tff',tff(i,j),tff(i,j+1),tff(i+1,j),tff(i+1,j+1)
rslt(i,j)= fuel_left_cell_3(  &
    lff(i,j),lff(i,j+1),lff(i+1,j),lff(i+1,j+1), &
    tff(i,j),tff(i,j+1),tff(i+1,j),tff(i+1,j+1),&
    time_now, fuel_time_cell)
write(*,*)'result after step i and j',rslt(i,j),i,j
end do
end do


print*,""
print*,""
!!!! Test #2

write(*,*)'MAIN TEST #2'
print*,"Check for consistency, for 3 inner cells with the same results"
time_now=2.
ims=1
ime=5
jms=1
jme=5

data lfn3/-1., -1.,-1., &
         -1., -1.,0., &
         0., 0.,0., &
         0., 1., 1., &
         1.,1.,1./

data tign3/1.0, 1., 1., &
          1.0, 1., 2., &
          2.0, 2., 2.,  &
          2.,2.,2., &
          2.,2.,2./

do icl=2,4
        write(*,*)"icl,jcl",icl,jcl
       call tign_lfn_interpolation(time_now,icl,jcl,ims,ime,jms,jme, &
                                    tign3,lfn3,tff,lff)

d=1e20
d(1:5:2,1:5:2)=tign3(icl-1:icl+1,1:3)
d(2:4,2:4)=tff
!write(*,*)'tign and tff'
!write(*,'(5f7.3)')d
!write(*,*)' '
d(1:5:2,1:5:2)=lfn3(icl-1:icl+1,1:3)
d(2:4,2:4)=lff
!write(*,*)'lfn and lff'
!write(*,'(5f7.3)')d
!write(*,*)' '

fuel_time_cell= 8.235294 ;
        write(*,*)"Calculation of fuel_frac over 4 subcells"
result=0;
do i=1,2
   do j=1,2


result=fuel_left_cell_3(  &
    lff(i,j),lff(i,j+1),lff(i+1,j),lff(i+1,j+1), &
    tff(i,j),tff(i,j+1),tff(i+1,j),tff(i+1,j+1),&
    time_now, fuel_time_cell)
write(*,*)'result after step i and j',result,i,j

end do
end do
end do
print*,"If the results for each correspondent subcell are equal in all cells"
print*,"then the code is consistent"


 end program test
