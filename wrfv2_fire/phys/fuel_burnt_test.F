program fuel_burnt_test
use module_fr_sfire_core
use module_fr_sfire_phys
implicit none

!*** arguments for fuel_burnt
integer:: ids,ide,jds,jde,ims,ime,jms,jme
parameter(ims=1,ime=2,jms=1,jme=2)
real,dimension(ims:ime,jms:jme)::lfn,tign,fuel_time
real:: tnow,dx,dy
real,dimension(ims:ime,jms:jme)::fuel_burn,fuel_frac,fuel_frac_jm

!local
real,dimension(ims:ime,jms:jme)::lfn0
integer::i,iu,test_flag
real::off


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
 
end do

end program

