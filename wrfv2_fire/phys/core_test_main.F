! Test the dynamical core of the fire model
! Jan Mandel jmandel@ucar.edu August 2007

!   test driver for fire_core, arguments communicated by files


program core_test
use module_fr_sfire_core
use module_fr_sfire_util
 

implicit none
!*** variables
integer :: m1,m2,n1,n2,i,j,m1m,m2m,n1m,n2m,halo
real,allocatable,dimension(:,:):: &
    lfn,tign,fuel_time,frac_lost,frac_end,vx,vy,r
real:: rm1,rm2,rn1,rn2,time_start,time_end,fire_dx,fire_dy
integer::num_tiles
integer, dimension(100)::i_start,i_end,j_start,j_end

!*** executable

! read the inputs from a file
open(1,form='formatted',file='core_test_in.txt')
read(1,1)rm1,rm2,rn1,rn2,time_start,time_end,fire_dx,fire_dy
print *,rm1,rm2,rn1,rn2,time_start,time_end,fire_dx,fire_dy
m1=nint(rm1)  ! domain dimensions
n1=nint(rn1)
m2=nint(rm2)
n2=nint(rn2)
halo=0
m1m=m1-halo      ! memory dims
m2m=m2+halo
n1m=n1-halo
n2m=n2+halo
print *,'core_test:',m1,m2,n1,n2
allocate (lfn(m1m:m2m,n1m:n2m),tign(m1m:m2m,n1m:n2m), &
  vx(m1m:m2m,n1m:n2m),vy(m1m:m2m,n1m:n2m),            &
  fuel_time(m1m:m2m,n1m:n2m),r(m1m:m2m,n1m:n2m),  &
  frac_lost(m1m:m2m,n1m:n2m),frac_end(m1m:m2m,n1m:n2m))
read(1,1)((lfn(i,j),i=m1,m2),j=n1,n2)
read(1,1)((tign(i,j),i=m1,m2),j=n1,n2)
read(1,1)((fuel_time(i,j),i=m1,m2-1),j=n1,n2-1)
read(1,1)((r(i,j),i=m1,m2),j=n1,n2)
read(1,1)((vx(i,j),i=m1,m2),j=n1,n2)
read(1,1)((vy(i,j),i=m1,m2),j=n1,n2)
close(1)

call set_tiles(1,1,m1,m2-1,n1,n2-1,num_tiles,i_start,i_end,j_start,j_end)

! run the code

call sfire_core( m1,m2-1,n1,n2-1,m1m,m2m,n1m,n2m,          &
num_tiles,i_start,i_end,j_start,j_end, &
time_start,time_end-time_start,fire_dx,fire_dy,fuel_time,           & 
lfn,tign,frac_end,frac_lost &
#ifdef SPEED_VARS_ARGS      /* extra arguments for normal_spread */
#include SPEED_VARS_ARGS
#endif
)

! write the output to a file

open (1,form='formatted',file='core_test_out.txt')
write(1,1)((lfn(i,j),i=m1,m2),j=n1,n2)
write(1,1)((tign(i,j),i=m1,m2),j=n1,n2)
write(1,1)((frac_lost(i,j),i=m1,m2-1),j=n1,n2-1)
write(1,1)((frac_end(i,j),i=m1,m2-1),j=n1,n2-1)
close(1)

1   format((e25.15,1x)) 

end program core_test
