module module_fr_spread_util

contains


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

end module module_fr_spread_util
