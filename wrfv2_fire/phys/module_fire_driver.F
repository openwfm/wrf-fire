!WRF:MEDIATION_LAYER:PHYSICS
!

MODULE module_fire_driver
CONTAINS

!------------------------------------------------------------------

   SUBROUTINE fire_driver(ifire                                 &
            ,u,v,u_frame,v_frame,mu,rho                         & ! send
            ,itimestep,dt,dx,dy,z,z_at_w,dz8w,ht                &
            ,ids,  ide,  kds,  kde,  jds,  jde                  & 
            ,ims,  ime,  kms,  kme,  jms,  jme                  &
            ,ips,  ipe,  kps,  kpe,  jps,  jpe                  &
            ,ifds, ifde, kfds, kfde, jfds, jfde                 &
            ,ifms, ifme, kfms, kfme, jfms, jfme                 &
            ,ifps, ifpe, kfps, kfpe, jfps, jfpe                 & 
            ,kts,kte,num_tiles,i_start,i_end,j_start,j_end      & 
            ,grid_id,cen_lat,cen_lon,lat_ll,lon_ll              &
            ,moad_cen_lat,moad_cen_lon                          &
            ,moad_lat_ll,moad_lon_ll,moad_dx,moad_dy            &
            ,moad_s_we,moad_e_we,moad_s_sn,moad_e_sn            &
            ,sr_x,sr_y                                          &
            ,fire_lat_init,fire_lon_init,fire_ign_time          &
            ,fire_shape,fire_crwn_hgt                           &
            ,fire_ext_grnd,fire_ext_crwn,fire_sprd_mdl          &
            ,fire_fuel_read,fire_fuel_cat                       &
            ,nfuel_cat,nfl,nfl_t,nfl_c,ncod,in1,in2,ixb,iyb     & ! send&recv
            ,icn,fg,fc,r_0,bbb,betafl,phiwc,area,area2          &
            ,zf,zsf,tign_g,tign_c,tign_crt,xfg,yfg,xcd          &
            ,ycd,xcn,ycn,sprdx,sprdy                            &
            ,rthfrten,rqvfrten                                  &
            ,lfn,fuel_frac                                      & ! i/o sfire
            ,grnhfx,grnqfx,canhfx,canqfx                        & ! recv
            ,fuel_time,dzfsdx,dzfsdy,fgip                       & 
            ,ischap                                          )    ! data


!------------------------------------------------------------------

   USE module_state_description, ONLY :                         &
                   FIRE_CAWFE,FIRE_SFIRE

   USE module_model_constants

! --- add new modules of schemes here

   USE module_fr_cawfe
   USE module_fr_sfire_driver

   !  This driver calls subroutines for the fire parameterizations.
   !
   !  fire scheme:
   !      1. CAWFE scheme (Clark, Coen, Latham  2004)
   !

!------------------------------------------------------------------
   IMPLICIT NONE
!======================================================================
! Grid structure in physics part of WRF
!----------------------------------------------------------------------
! The horizontal velocities used in the physics are unstaggered
! relative to temperature/moisture variables. All predicted
! variables are carried at half levels except w, which is at full
! levels. Some arrays with names (*8w) are at w (full) levels.
!
!----------------------------------------------------------------------
! In WRF, kms (smallest number) is the bottom level and kme (largest
! number) is the top level.  In your scheme, if 1 is at the top level,
! then you have to reverse the order in the k direction.
!
!         kme      -   half level (no data at this level)
!         kme    ----- full level
!         kme-1    -   half level
!         kme-1  ----- full level
!         .
!         .
!         .
!         kms+2    -   half level
!         kms+2  ----- full level
!         kms+1    -   half level
!         kms+1  ----- full level
!         kms      -   half level
!         kms    ----- full level
!
!======================================================================
! Definitions
!-----------
!-- rthfrten      Theta tendency due to fire parameterization (K/s)
!-- rqvfrten      Qv tendency due to fire parameterization (kg/kg/s)
!-- itimestep     number of time steps
!-- z             height above sea level (m)
!-- dx            horizontal space interval (m)
!-- dt            time step (second)
!-- zs
!-- ids           start index for i in domain
!-- ide           end index for i in domain
!-- jds           start index for j in domain
!-- jde           end index for j in domain
!-- kds           start index for k in domain
!-- kde           end index for k in domain
!-- ims           start index for i in memory
!-- ime           end index for i in memory
!-- jms           start index for j in memory
!-- jme           end index for j in memory
!-- kms           start index for k in memory
!-- kme           end index for k in memory
!-- jts           start index for j in tile
!-- jte           end index for j in tile
!-- kts           start index for k in tile
!-- kte           end index for k in tile
!
!******************************************************************
!------------------------------------------------------------------ 
!
   INTEGER, INTENT(in) :: ifire

   INTEGER, INTENT(in) :: ifds,ifde, jfds,jfde, kfds,kfde ! strt/end index domain
   INTEGER, INTENT(in) :: ifms,ifme, jfms,jfme, kfms,kfme ! strt/end index memory
   INTEGER, INTENT(in) :: ifps,ifpe, jfps,jfpe, kfps,kfpe ! strt/end index patch

   INTEGER, INTENT(in) :: ids,ide, jds,jde, kds,kde  ! atmosphere domain dimensions
   INTEGER, INTENT(in) :: ims,ime, jms,jme, kms,kme  ! atmosphere memory dimensions
   INTEGER, INTENT(in) :: ips,ipe, kps,kpe, jps,jpe  ! atmosphere patch dimensions 
   INTEGER, INTENT(in) :: kts,kte, num_tiles         ! atmosphere tile dimensions

   INTEGER, DIMENSION(num_tiles), INTENT(in) ::                   &
                                       i_start,i_end,j_start,j_end

   INTEGER, INTENT(in) :: itimestep                  ! current time step (cumultiv)
   REAL,    INTENT(in) :: dt                         ! time step
   REAL,    INTENT(in) :: dx,dy                      ! dx,dy on innermost atm mesh
   INTEGER, INTENT(in) :: grid_id                    ! grid id of innermost atm mesh
   REAL, INTENT(in)    :: cen_lat,cen_lon            ! center lat,lon of " " "
   REAL, INTENT(in)    :: lat_ll,lon_ll              ! lat,lon of sw corner of " " "
   REAL, INTENT(in)    :: moad_cen_lat,moad_cen_lon  ! lat,lon of center of moad
   REAL, INTENT(in)    :: moad_lat_ll,moad_lon_ll    ! lat,lon of sw corner of moad
   REAL, INTENT(in)    :: moad_dx,moad_dy            ! dx,dy of moad
   INTEGER, INTENT(in) :: moad_s_we,moad_e_we        ! strt/stop grd pts in x moad
   INTEGER, INTENT(in) :: moad_s_sn,moad_e_sn        ! strt/stop grd pts in y moad

   INTEGER, INTENT(in) :: sr_x,sr_y
   REAL, INTENT(in)    :: fire_lat_init,fire_lon_init
   REAL, INTENT(in)    :: fire_ign_time
   INTEGER, INTENT(in) :: fire_fuel_read
   INTEGER, INTENT(in) :: fire_shape
   REAL, INTENT(in)    :: fire_crwn_hgt
   REAL, INTENT(in)    :: fire_ext_crwn
   REAL, INTENT(in)    :: fire_ext_grnd
   INTEGER, INTENT(in) :: fire_sprd_mdl
   INTEGER, INTENT(in) :: fire_fuel_cat
   REAL, INTENT(in)    :: u_frame, v_frame

   REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: u,v
   REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: z,z_at_w
   REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: rho
   REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: dz8w
   REAL, INTENT(in), DIMENSION( ims:ime, jms:jme ) :: ht
   REAL, INTENT(in), DIMENSION( ims:ime, jms:jme ) :: mu

! ----- inout variables

   INTEGER, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: nfuel_cat
   INTEGER, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: nfl,nfl_t,nfl_c
   INTEGER, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: ncod
   INTEGER, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,2 ) :: in1,in2
   INTEGER, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: ixb,iyb,icn

   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: fg,fc
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: r_0,bbb,betafl,phiwc
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: area,area2
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: zf,zsf
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: tign_g,tign_c,tign_crt
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xfg,yfg
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xcd,ycd
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xcn,ycn
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: sprdx,sprdy
 
   REAL, INTENT(inout), DIMENSION( ims:ime, kms:kme, jms:jme ) ::  &
                                                         rthfrten, &
                                                         rqvfrten

   REAL, INTENT(out), DIMENSION( ims:ime,jms:jme ) :: grnhfx,grnqfx
   REAL, INTENT(out), DIMENSION( ims:ime,jms:jme ) :: canhfx,canqfx

   REAL, INTENT(inout),DIMENSION( ifms:ifme,jfms:jfme ) :: &
                                lfn,fuel_frac,fuel_time,dzfsdx,dzfsdy,fgip
   INTEGER,intent(inout),DIMENSION( ifms:ifme,jfms:jfme ) :: ischap


! ---- local variables

   INTEGER :: i,j,k,nk,jj,ij,its,ite,jts,jte

   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: v_tmp
   REAL, DIMENSION( ims:ime, kms:kme, jms:jme ) :: u_tmp
   CHARACTER(LEN=128) :: msg

!------------------------------------------------------------------
!
    CALL wrf_debug(100,'entering fire_driver')

! -- get u and v, zero tendencies

   !$OMP PARALLEL DO   &
   !$OMP PRIVATE ( ij,i,j,k )
   DO ij = 1 , num_tiles
      DO j=j_start(ij),j_end(ij)
         DO k=kts,kte
            DO i=i_start(ij),i_end(ij)
               v_tmp(i,k,j) = v(i,k,j) + v_frame
               u_tmp(i,k,j) = u(i,k,j) + u_frame
            ENDDO
         ENDDO
         DO k=kts,min(kte+1,kde)
            DO i=i_start(ij),i_end(ij)
               rthfrten(i,k,j)=0.
               rqvfrten(i,k,j)=0.
            ENDDO
         ENDDO
      ENDDO
   ENDDO
   !$OMP END PARALLEL DO
!
   fire_select: SELECT CASE(ifire)

   CASE (FIRE_CAWFE)

     CALL wrf_debug(100,'entering CAWFE fire scheme')
     !$OMP PARALLEL DO   &
     !$OMP PRIVATE ( ij, i,j,k, its, ite, jts, jte )
     DO ij = 1 , num_tiles

      its = i_start(ij)   ! start atmospheric tile in i
      ite = i_end(ij)     ! end atmospheric tile in i
      jts = j_start(ij)   ! start atmospheric tile in j
      jte = j_end(ij)     ! end atmospheric tile in j


        CALL cawfe(                                             &
             ids,ide, kds,kde, jds,jde                          & ! send
            ,ims,ime, kms,kme, jms,jme                          &
            ,its,ite, kts,kte, jts,jte                          &
            ,ifds,ifde, kfds,kfde, jfds,jfde                    &
            ,ifms,ifme, kfms,kfme, jfms,jfme                    &
            ,itimestep,dt,dx,dy                                 &
            ,grid_id,cen_lat,cen_lon,lat_ll,lon_ll              &
            ,moad_cen_lat,moad_cen_lon                          &
            ,moad_lat_ll,moad_lon_ll,moad_dx,moad_dy            &
            ,moad_s_we,moad_e_we,moad_s_sn,moad_e_sn            &
            ,sr_x,sr_y                                          &
            ,fire_lat_init,fire_lon_init,fire_ign_time/60.      &
            ,fire_shape,fire_sprd_mdl,fire_crwn_hgt             &
            ,fire_ext_grnd,fire_ext_crwn                        &
            ,fire_fuel_read,fire_fuel_cat                       &
            ,z,z_at_w,dz8w,ht,u_tmp,v_tmp,mu,rho                &
            ,nfuel_cat,nfl,nfl_t,nfl_c,ncod,in1,in2,ixb,iyb     & ! send&recv
            ,icn,fg,fc,r_0,bbb,betafl,phiwc,area,area2          &
            ,zf,zsf,tign_g,tign_c,tign_crt,xfg,yfg,xcd          &
            ,ycd,xcn,ycn,sprdx,sprdy                            &
!            ,rthfrten,rqvfrten)                                 
            ,rthfrten,rqvfrten                                  &
            ,grnhfx,grnqfx,canhfx,canqfx)                         ! recv

     ENDDO
     !$OMP END PARALLEL DO

     CALL wrf_debug(100,'exiting CAWFE fire scheme')

   CASE (FIRE_SFIRE)

     CALL wrf_debug(100,'entering SFIRE fire scheme')

     ! sfire_driver is in mediation layer, will have its own loops over tile inside
     ! do NOT call from OMP loop!!

     CALL sfire_driver(                                      &
             ids,ide, kds,kde, jds,jde                          &
            ,ims,ime, kms,kme, jms,jme                          &
            ,ips,ipe, kps,kpe, jps,jpe                          & 
            ,ifds,ifde, jfds,jfde                    &
            ,ifms,ifme, jfms,jfme                    &
            ,ifps,ifpe, jfps,jfpe                    &
            ,sr_x,sr_y                                          &
            ,num_tiles,i_start,i_end,j_start,j_end              &
            ,itimestep                                          &
            ,fire_fuel_read,fire_fuel_cat                       &
            ,dt,dx,dy                                           &
            ,u_frame,v_frame                                    &
            ,fire_ext_grnd,fire_ext_crwn,fire_crwn_hgt          &
            ,u,v,mu,rho,ht                                      &
            ,z_at_w,dz8w                                        &
            ,lfn,tign_c,fuel_frac                     & 
            ,rthfrten,rqvfrten                                  &
            ,grnhfx,grnqfx,canhfx,canqfx                        &
            ,nfuel_cat                                          &
            ,fuel_time,zsf                                      &
            ,dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip,ischap     &
            )

        CALL wrf_debug(100,'exiting SFIRE fire scheme')
   CASE DEFAULT

       WRITE( msg , * ) 'This fire option does not exist: ifire = ', ifire
       CALL wrf_error_fatal ( msg )

   END SELECT fire_select

   ! common print to compare
   ! NOTE: this will not work even on OMP - debug only
   if(num_tiles.ne.1)call wrf_error_fatal('fire_driver:print_3d_stats debug print supported on 1 tile only')
   call print_2d_stats(ids,ide,jds,jde,ims,ime,jms,jme,grnhfx,'fire_driver:grnhfx')
   call print_2d_stats(ids,ide,jds,jde,ims,ime,jms,jme,grnqfx,'fire_driver:grnqfx')
   call print_3d_stats(ids,ide,kds,kde,jds,jde,ims,ime,kms,kme,jms,jme,rthfrten,'fire_driver:rthfrten')
   call print_3d_stats(ids,ide,kds,kde,jds,jde,ims,ime,kms,kme,jms,jme,rqvfrten,'fire_driver:rqvfrten')

   END SUBROUTINE fire_driver

END MODULE module_fire_driver
