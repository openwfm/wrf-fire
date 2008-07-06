program fuel_burnt_test
use module_fr_sfire_core
use module_fr_sfire_phys
<<<<<<< HEAD:wrfv2_fire/phys/fuel_burnt_test.F
implicit none

! ims,ime,jms,jme   in   memory dimensions
! its,ite,jts,jte   in   tile dimensions (cells where fuel_frac computed)
! ifs,ife,jfs,jfe   in   fuel_frac memory dimensions
! ir,jr,            in   refinement - quadrature cells per fire cell, even, at least 2
! lfn               in   level function, at nodes at midpoints of cells
! tign              in   ignition time, at nodes at nodes at midpoints of cells
! fuel_time         in   time constant of fuel, per cell
! time_now          in   time now
! fuel_frac         out  fraction of fuel remaining, per cell

!*** local
integer:: ids,ide,jds,jde,ims,ime,jms,jme,ir,jr,k
parameter(ims=1,ime=2,jms=1,jme=2,ir=2,jr=2)
real:: time_now,off,fuel_left_f
real,dimension(ims:ime,jms:jme)::lff,tif,lfn0,fuel_time
integer::i,iu
=======
implicit none
>>>>>>> a94a96b719ea41bf0825215d4c27b62105f663f0:wrfv2_fire/phys/fuel_burnt_test.F

<<<<<<< HEAD:wrfv2_fire/phys/fuel_burnt_test.F
lfn0(1,1)=0.419279 
lfn0(1,2)=-1.9766893E-02
lfn0(2,1)=6.310914  
lfn0(2,2)=5.983462
time_now=2.0
fuel_time(1,1)= 8.23529
=======
!*** arguments for fuel_burnt
integer:: ids,ide,jds,jde,ims,ime,jms,jme
parameter(ims=1,ime=2,jms=1,jme=2)
real,dimension(ims:ime,jms:jme)::lfn,tign,fuel_time
real:: tnow,dx,dy
real,dimension(ims:ime,jms:jme)::fuel_burn,fuel_frac,fuel_frac_jm
>>>>>>> a94a96b719ea41bf0825215d4c27b62105f663f0:wrfv2_fire/phys/fuel_burnt_test.F

<<<<<<< HEAD:wrfv2_fire/phys/fuel_burnt_test.F
do k=1,2
=======
!local
real,dimension(ims:ime,jms:jme)::lfn0
integer::i,iu,test_flag
real::off
>>>>>>> a94a96b719ea41bf0825215d4c27b62105f663f0:wrfv2_fire/phys/fuel_burnt_test.F

<<<<<<< HEAD:wrfv2_fire/phys/fuel_burnt_test.F
  off = (k-1)*0.05
  lff(1,1)=lfn0(1,1)-off
  lff(1,2)=lfn0(1,2)-off
  lff(2,1)=lfn0(2,1)-off
  lff(2,2)=lfn0(2,2)-off

  tif(1,1)=2.000000
  tif(1,2)=1.565244
  tif(2,1)=2.000000
  tif(2,2)=2.000000
 
  iu=1
  open(iu,file='tmp.txt')
  fuel_left_f=fuel_left_cell( &
                 lff(1,1),lff(1,2),lff(2,1),lff(2,2),&
                 tif(1,1),tif(1,2),tif(2,1),tif(2,2),&
                 time_now, fuel_time(1,1)) 
                                              ! premultiply by weight
  print *,'fuel_left_f=',fuel_left_f 
=======
>>>>>>> a94a96b719ea41bf0825215d4c27b62105f663f0:wrfv2_fire/phys/fuel_burnt_test.F

<<<<<<< HEAD:wrfv2_fire/phys/fuel_burnt_test.F
  write(iu,*) fuel_left_f 
  close(iu)    
=======
ids = 1
ide = 1
jds = 1
jde = 1
dx=0.5
dy=0.5
lfn0(1,1)=0
lfn0(1,2)=0.3479
lfn0(2,1)=0.6521
lfn0(2,2)=1.0
tnow=1.0
fuel_time(1,1)=1
do i=1,23
  off = (i-1)*0.05
  lfn(1,1)=lfn0(1,1)-off
  lfn(1,2)=lfn0(1,2)-off
  lfn(2,1)=lfn0(2,1)-off
  lfn(2,2)=lfn0(2,2)-off

  tign(1,1)=lfn(1,1)*2+tnow
  tign(1,2)=lfn(1,2)*2+tnow
  tign(2,1)=lfn(2,1)*2+tnow
  tign(2,2)=lfn(2,2)*2+tnow
  
  iu=1
  open(iu,file='tmp.txt')
  call fuel_burnt(ids,ide,jds,jde,dx,dy,&
                     ims,ime,jms,jme, &
                     lfn, tign, fuel_time, tnow, fuel_burn)
  print *,'fuel burnt=',fuel_burn(1,1)    
  write(iu,*) fuel_burn(1,1)  
  close(iu)
  
!  call fuel_left(ids,ide,jds,jde,dx,dy,&
!                     ims,ime,jms,jme, &
!                     lfn, tign, fuel_time, tnow, fuel_frac)
!  print *,'off,fuel burnt=',off,fuel_frac(1,1)
!  call fuel_left_jm(ids,ide,jds,jde,dx,dy,&
!                     ims,ime,jms,jme, &
!                     lfn, tign, fuel_time, tnow, fuel_frac_jm)                     
!  print *,'off,fuel burnt=',off,fuel_frac_jm(1,1)                   
>>>>>>> a94a96b719ea41bf0825215d4c27b62105f663f0:wrfv2_fire/phys/fuel_burnt_test.F
 
end do

end program

