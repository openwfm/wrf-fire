module module_fr_sfire_burn

contains

subroutine fuel_left(ids,ide,jds,jde,ims,ime,jms,jme,lfn,tign,fuel_time,tnow,fuel_frac)
use module_fr_sfire_util
implicit none

!*** purpose: determine fraction of fuel remaining

!*** Jan Mandel August 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com

!*** arguments

integer, intent(in) :: ids,ide,jds,jde,ims,ime,jms,jme
real, intent(in), dimension(ims:ime,jms:jme)::lfn,tign,fuel_time
real, intent(in):: tnow
real, intent(out), dimension(ims:ime,jms:jme)::fuel_frac

! ids,ide,jds,jde   in   mesh domain dimensions (cells)      (1)
! ims,ime,jms,jme   in   mesh aray dimensions                (1)
! lfn               in   level function, at nodes
! tign              in   ignition time, at nodes
! fuel_time         in   time constant of fuel, per cell
! tnow              in   time now
! fuel_frac         out  fraction of fuel remaining

!*** Description
! The area burning is given by the condition P(x,y) <= 0, where the function P is
! interpolated from the values of lfn at mesh nodes,
! P(dx*(i-1),dy*(j-1))=lfn(i,j).
!
! The time since ignition in location (x,y) is the function T, interpolated in 
! each mesh cell from the values T(dx*(i-1),dy*(j-1))=tign(i,j) at the nodes
! where lfn(i,j)<=0, and T(x,y)=tnow on all points on the grid lines where P(x,y) = 0.
! The values of tign(i,j) where lfn(i,j)>0 are ignored.
!
! The subroutine computes for each mesh cell [dx*(i-1),dx*i] by [dy*(j-1),dy*j]
! an approximation of the average of exp(-T(x,y)/w(i,j)) over the burning area
! in the cell, that is an approximation of the integral
!
!                              /\
!                     1        |            T(x,y)-tnow
! fuel_frac(i,j)  =  -----     |    exp( -  ------------ ) dxdy
!                    dx*dy     |              w(i,j)
!                             \/
!                       dx*(i-1)<x<dx*(i+1)
!                       dy*(j-1)<y<dy*(j+1)
!                        lfn(x,y)<=0
!
! When the cell is not burning at all (all lfn>=0), then fuel_frac(i,j)=1.
! Because of symmetries, the result should not depend on the mesh spacing dx dy
! so dx and dy are not in the argument list.
!
! Example:
!
!        lfn<0         lfn>0
!      (i,j+1)-----O--(i+1,j+1)            O = points on the fireline, T=tnow
!            |      \ |                    A = the burning area for computing
!            |       \|                        fuel_frac(i,j)
!            |   A    O 
!            |        |
!            |        |
!       (i,j)---------(i+1,j)
!       lfn<0          lfn<0
!
! Approximations allowed: 
! The fireline can be approximated by straight line(s).
! When all cell is burning, approximation by 1 point Gaussian quadrature is OK.
! 
! Requirements:
! The output should be a continuous function of the arrays lfn and tign whenever 
! lfn(i,j)=0 implies tign(i,j)=tnow.  
! The output should be invariant to the symmetries of the input in each cell.
! Arbitrary combinations of the signs of lfn(i,j) should work.
! The result should be at least 1st order accurate, i.e. exact when 
! exp(T) is replaced by a linear function equal to one at the fireline
!
! IMPORTANT: follow WRF coding conventions 
! http://www.mmm.ucar.edu/wrf/WG2/WRF_conventions.html

!*** local

integer::i,j
real,dimension(ims:ime,jms:jme)::t,ap
real:: ta,aps,ps,a

! a very crude approximation - replace by a better code

call check_mesh_2dim(ids,ide+1,jds,jde+1,ims,ime,jms,jme)

do j=jds,jde+1     ! note the order of indices for fast  execution
    do i=ids,ide+1  
        if (lfn(i,j)>0) then
            t(i,j)=1.0  ! add missing values - should be > tnow what the hell
        else
            t(i,j)=exp(tign(i,j)-tnow)
        endif
        ap(i,j)=abs(lfn(i,j))
    enddo
enddo

do j=jds,jde
    do i=ids,ide ! it is OK to introduce extra scalars, just as fast
    
        ! average unscaled fuel fraction since ignition
        ta=0.25*(t(i+1,j+1)+t(i+1,j)+t(i,j+1)+t(i,j))  
        ps=lfn(i+1,j+1)+lfn(i+1,j)+lfn(i,j+1)+lfn(i,j)   
        aps=ap(i+1,j+1)+ ap(i+1,j)+ ap(i,j+1)+ ap(i,j)
        
        ! a=0 if all lfn>0, 1 if all <0, transition except when all lfn=0
        if (aps>0.0 .or. aps<0.0) then  ! never compare =0.0, slow
               a=(-ps/aps+1.0)*0.5
        else ! for fast code, the else clause should happen less often
               a=0.5d0 ! just to have something, if all lfn=0 it is junk anyway
        endif
        
        fuel_frac(i,j)=a*ta**(1.0/fuel_time(i,j))+(1.0-a)
    enddo
enddo

end subroutine fuel_left

end module module_fr_sfire_burn
