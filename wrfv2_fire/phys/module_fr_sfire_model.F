module module_fr_spread_model
!
!*** Jan Mandel September 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
contains

subroutine fire_spread_model &
                (ids,ide, kds,kde, jds,jde,                    & ! incoming
                 ims,ime, kms,kme, jms,jme,                    &
                 its,ite, kts,kte, jts,jte,                    &
                 ifds,ifde, kfds,kfde, jfds,jfde,              &
                 ifms,ifme, kfms,kfme, jfms,jfme,              &
                 itimestep,dt,dx,dy,                           &
                 grid_id,cen_lat,cen_lon,lat_ll,lon_ll,        &
                 moad_cen_lat,moad_cen_lon,                    &
                 moad_lat_ll,moad_lon_ll,moad_dx,moad_dy,      &
                 moad_s_we,moad_e_we,moad_s_sn,moad_e_sn,      &
                 nfrx,nfry,                                    &
                 tlat_stf,tlon_stf,t_ignite,ishape,ibeh,       &
                 z1can,alfg,alfc,ifuelread,nfuel_cat0,         &
                 z,z_at_w,dz8w,zs,u,v,mu,rho,                  &
                 nfuel_cat,nfl,nfl_t,nfl_c,ncod,               & ! in and out
                 in1,in2,ixb,iyb,icn,                          &
                 fg,fc,r_0,bbb,betafl,phiwc,area,area2,        &
                 zf,zsf,tign_g,tign_c,tign_crt,                &
                 xfg,yfg,xcd,ycd,xcn,ycn,sprdx,sprdy,          &
                 rthfrten,rqvfrten,                            & ! outgoing
                 grnhfx,grnqfx,canhfx,canqfx)  ! temp? 


use module_fr_spread_core
use module_fr_spread_util
implicit none

!*** arguments

INTEGER, INTENT(in) :: ids,ide, kds,kde, jds,jde    ! atmosphere domain indices
INTEGER, INTENT(in) :: ims,ime, kms,kme, jms,jme    ! atmosphere memory indices
INTEGER, INTENT(in) :: its,ite, kts,kte, jts,jte    ! atmosphere tile   indices
 
INTEGER, INTENT(in) :: ifds,ifde, jfds,jfde, kfds,kfde  ! fire domain indices
INTEGER, INTENT(in) :: ifms,ifme, jfms,jfme, kfms,kfme  ! fire memory indices

INTEGER, INTENT(in) :: itimestep         ! current time step (cumulative)
REAL,    INTENT(in) :: dt                ! time step
REAL,    INTENT(in) :: dx,dy             ! dx,dy on innermost atm mesh

! incoming atmos. winds (m/s at arakawa-c grid locations)
REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: u,v
! height above sea level of w points (m)
REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: z,z_at_w
 

! local
integer::if_st,if_en,jf_st,jf_en

write(*,7001) &
                 ids,ide, kds,kde, jds,jde,                    & ! incoming
                 ims,ime, kms,kme, jms,jme,                    &
                 its,ite, kts,kte, jts,jte,                    &
                 ifds,ifde, kfds,kfde, jfds,jfde,              &
                 ifms,ifme, kfms,kfme, jfms,jfme
7001 format('FR: indices to cawfe:'/'ids ',6i8/'ims ',6i8/'idfs ',6i8/'imfs ',6i8)

   CALL fire_winds(u,v,                       &  ! in
                   ids,ide, kds,kde, jds,jde, &
                   ims,ime, kms,kme, jms,jme, &
                   its,ite, kts,kte, jts,jte, &
                   sfcu,sfcv)                    ! out

! ----- get the time from model start (assumes non-variable dt)
 
   time = FLOAT(itimestep) * dt

! ----- set indicies over which fire grid exists
!
!       these indicies are needed so that we are properly handling
!       fire calculations within tiles that butt up against domain
!       boundaries where the halo information is not available.  
!       therefore the fire exists on one less atmospheric grid point 
!       than the innermost domain is dimensioned.

   if_st = MAX( (its-1)*nfrx+1, ids*nfrx+1   )
   if_en = MIN( (ite)*nfrx    , (ide-1)*nfrx )
   jf_st = MAX( (jts-1)*nfry+1, jds*nfry+1   ) 
   jf_en = MIN( (jte)*nfry    , (jde-1)*nfry )

! ----- begin initialization for all runs, including restart
 


end subroutine fire_spread_model

end module module_fr_spread_model
