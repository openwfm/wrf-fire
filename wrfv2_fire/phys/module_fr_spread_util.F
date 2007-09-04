module module_fr_spread_util

contains

!
!****************
!


subroutine sum_2dmesh_cells(                      &
       ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2,&  ! input
       ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1)   ! output
use module_fr_wrfstubs
implicit none

!*** purpose
! sum cell values in mesh2 to cell values of coarser mesh1

!*** arguments
! the dimensions are in cells, not nodes!

integer, intent(in)::ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)

!*** local
integer:: i1,i2,j1,j2,ir,jr,isz1,isz2,jsz1,jsz2,ioff,joff
real t
character(len=128)msg

!*** executable

!check mesh dimensions and domain dimensions
call check_mesh_dim(ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1)
call check_mesh_dim(ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2)

! compute mesh sizes
isz1 = ide1-ids1+1
jsz1 = jde1-jds1+1
isz2 = ide2-ids2+1
jsz2 = jde2-jds2+1

! check mesh sizes
if(isz1.le.0.or.jsz1.le.0.or.isz2.le.0.or.jsz2.le.0)then
    call message('all mesh sizes must be positive')
    goto 9
endif
if(mod(isz2,isz1).ne.0.or.mod(jsz2,jsz1).ne.0)then
    call message('input mesh size must be multiple of output mesh size')
    goto 9
endif

! compute mesh ratios
ir=isz2/isz1
jr=jsz2/jsz1

! v1 = v1 + sum(v2)
do j1=jds1,jde1
    do i1=ids1,ide1
       t=0
       do joff=0,jr-1
           do ioff=0,ir-1
               i2=ioff+ids2+ir*(i1-ids1)
               j2=joff+jds2+jr*(j1-jds1)
               t=t+v2(i2,j2)
           enddo
       enddo
       v1(i1,j1)=t
    enddo
enddo

return
9 continue
write(msg,91)ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
call message(msg)
write(msg,91)ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
call message(msg)
write(msg,92)'input  mesh size:',isz2,jsz2
call message(msg)
91 format('dimensions: ',8i8)
write(msg,92)'output mesh size:',isz1,jsz1
call message(msg)
92 format(a,2i8)
call crash('module_fr_spread_util:sum_mesh_cells: bad mesh sizes')
end subroutine sum_2dmesh_cells

!
!****************
!

subroutine interpolate_2dmesh_nodes(              &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  ! in  
      ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1  )  ! out
use module_fr_wrfstubs
implicit none

!*** purpose
! interpolate nodal values in mesh2 to nodal values in mesh1
! input mesh 2 is coarse output mesh 1 is fine

!*** arguments

integer, intent(in)::ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)

!*** local
integer:: i1,i2,j1,j2,ir,jr,isz1,isz2,jsz1,jsz2,ioff,joff
real:: tx,ty,rx,ry
character(len=128)msg

!*** executable

!check mesh dimensions and domain dimensions
call check_mesh_dim(ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1)
call check_mesh_dim(ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2)

! compute mesh sizes
isz1 = ide1-ids1
jsz1 = jde1-jds1
isz2 = ide2-ids2
jsz2 = jde2-jds2

! check mesh sizes
if(isz1.le.0.or.jsz1.le.0.or.isz2.le.0.or.jsz2.le.0)goto 9
if(mod(isz1,isz2).ne.0.or.mod(jsz1,jsz2).ne.0)goto 9

! compute mesh ratios
ir=isz1/isz2
jr=jsz1/jsz2
rx=ir
ry=jr

! v1 = v1 + interpolate(v2)
! this loop goes over coarse mesh lines twice but avoid complications
do j2=jds2,jde2-1
    do i2=ids2,ide2-1
        do ioff=0,ir
            do joff=0,jr
                ! compute fine mesh coordinate
                i1=ioff+ids1+ir*(i2-ids2)
                j1=joff+jds1+jr*(j2-jds2)
                ! weights
                tx = ioff/rx
                ty = joff/ry
                ! interpolation
                v1(i1,j1)=                     &
                      (1-tx)*(1-ty)*v2(i2,j2)  &
                 +    tx*(1-ty)  *v2(i2,j2+1)  &
                 +    (1-tx)*ty  *v2(i2+1,j2)  &
                 +        tx*ty  *v2(i2+1,j2+1)  
        !print *,'coarse ',i2,j2,' fine ',i1,j1, 'weights ',tx,ty, &
                ! 'in ',v2(i2,j2),v2(i2,j2+1),v2(i2+1,j2),v2(i2+1,j2+1),' out ',v1(i1,j1)
           enddo
       enddo
    enddo
enddo

return
9 continue
write(msg,91)ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
call message(msg)
write(msg,91)ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
call message(msg)
write(msg,92)'input  mesh size:',isz2,jsz2
call message(msg)
91 format('dimensions: ',8i8)
write(msg,92)'output mesh size:',isz1,jsz1
call message(msg)
92 format(a,2i8)
call crash("module_fr_spread_util:interpolate_mesh_nodes: bad mesh sizes")
end subroutine interpolate_2dmesh_nodes

!
!****************
!

subroutine check_mesh_dim(ids,ide,jds,jde,ims,ime,jms,jme)
use module_fr_wrfstubs
integer, intent(in)::ids,ide,jds,jde,ims,ime,jms,jme
if(ids<ims.or.ide>ime.or.jds<jms.or.jde>jme)call crash('domain dimensions too large')
end subroutine check_mesh_dim

!
!****************
!
                
real function interp(ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,ip1,jp1,x1,y1,dx1,dy1,v1,x,y)
use module_fr_wrfstubs
implicit none
!*** purpose
! general interpolation in a mesh aligned with coordinates

!*** arguments
! the mesh is given by the position (x1,y1) of the node (ip1,ip2) and the spacing (dx1,dy1)
! the coordinates to interpolate to are x y 
! returns the interpolation from v1 at mesh nodes

integer, intent(in)::ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,ip1,jp1
real, intent(in)::x1,y1,dx1,dy1,v1(ims1:ime1,jms1:jme1),x,y

!*** calls
intrinsic floor

!*** local
integer i1,j1,i2,j2
real tx,ty,xx,yy

! executable

! indices of the lower left corner of the cell in mesh 1 that contains (x,y)
i1 = ip1+floor((x - x1)/dx1)
i1=max(min(i1,ide1-1),ids1)
j1 = jp1+floor((y - y1)/dy1)
j1=max(min(j1,jde1-1),jds1)

! position of node (i1,j1)
xx = x1 + dx1*(i1-ip1)
yy = y1 + dy1*(j1-jp1)

! the leftover
tx = x - xx
ty = y - yy
if(tx.gt.1.0)call crash('bad tx')
if(ty.gt.1.0)call crash('bad ty')

! interpolate the values
interp = &
                    (1-tx)*(1-ty)*v1(i1,j1)    &
                 +    tx*(1-ty)  *v1(i1+1,j1)  &
                 +    (1-tx)*ty  *v1(i1,j1+1)  &
                 +        tx*ty  *v1(i1+1,j1+1)  

print *,'x,y=',x,y,'i1,i2=',i1,j1,'tx,ty=',tx,ty,' interp=',interp
end function interp

!
!****************
!

SUBROUTINE fire_winds(u,v,                       &  ! incoming
                     ids,ide, kds,kde, jds,jde, &
                     ims,ime, kms,kme, jms,jme, &
                     its,ite, kts,kte, jts,jte, &
                     u_i,v_i)                      ! outgoing

! from module_fr_cawfe by Ned Patton
! --- this routine takes u and v from the arakawa c-grid and interpolates
!     them horizontally and upward to the w-level (i.e. to the grid cube corners)
!     as desired by the fire code.  note that the final values are two 
!     dimensional arrays that are six grid points tall valid at the w-levels 
!     and that the exterior single grid point on all four edges of the domain 
!     are not filled.
!
!                v(1,2)                u(1,2)            u(2,2)
!              ----*----               v(1,2) *--------* v(2,2)
!              |       |                      |        |
!      u(1,1)  *       * u(2,1)  ===>         |        |
!              |       |                      |        |
!  ^ y         ----*----               u(1,1) *--------* u(2,1)
!  |             v(1,1)                v(1,1)            v(2,1)
!  |
!  *----> x                             and shifted up to w-level
!
   IMPLICIT NONE

   INTEGER , INTENT(in) :: ids,ide, kds,kde, jds,jde, &
                           ims,ime, kms,kme, jms,jme, &
                           its,ite, kts,kte, jts,jte

   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: u,v

   REAL, INTENT(out), DIMENSION( ims:ime,jms:jme,6 ) :: u_i,v_i

   INTEGER :: i,j,k
   INTEGER :: i_st,i_en
   INTEGER :: j_st,j_en

   ! --- set indicies

   i_st = MAX(its,ids+1)
   i_en = MIN(ite,ide-1)
   j_st = MAX(jts,jds+1)
   j_en = MIN(jte,jde-1)

   ! --- get velocities

   DO k = 1,6
      DO j = j_st,j_en
      DO i = i_st,i_en
         u_i(i,j,k) = .25*( u(i,k,j) + u(i,k,j+1) + u(i,k+1,j) + u(i,k+1,j+1) )
         v_i(i,j,k) = .25*( v(i-1,k,j) + v(i,k,j) + v(i-1,k+1,j) + v(i,k+1,j) )
      END DO
      END DO
   END DO

   RETURN

END SUBROUTINE fire_winds

!
!****************
!

end module module_fr_spread_util
