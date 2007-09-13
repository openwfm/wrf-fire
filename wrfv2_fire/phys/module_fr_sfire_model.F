module module_fr_sfire_model
!
!*** Jan Mandel September 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
contains

subroutine sfire_model &
                (ids,ide, kds,kde, jds,jde,                    & ! incoming
                 ims,ime, kms,kme, jms,jme,                    &
                 its,ite, kts,kte, jts,jte,                    &
                 ifds,ifde, kfds,kfde, jfds,jfde,              &
                 ifms,ifme, kfms,kfme, jfms,jfme,              &
                 nfrx,nfry,                                    &
                 itimestep,dt,dx,dy,                           &
                 grid_id,cen_lat,cen_lon,lat_ll,lon_ll,        &
                 moad_cen_lat,moad_cen_lon,                    &
                 moad_lat_ll,moad_lon_ll,moad_dx,moad_dy,      &
                 moad_s_we,moad_e_we,moad_s_sn,moad_e_sn,      &
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


use module_fr_sfire_core
use module_fr_sfire_util
use module_fr_sfire_speed

implicit none

!*** arguments

INTEGER, INTENT(in) :: ids,ide, kds,kde, jds,jde    ! atmosphere domain indices
INTEGER, INTENT(in) :: ims,ime, kms,kme, jms,jme    ! atmosphere memory indices
INTEGER, INTENT(in) :: its,ite, kts,kte, jts,jte    ! atmosphere tile   indices
 
INTEGER, INTENT(in) :: ifds,ifde, jfds,jfde  ! fire domain indices
INTEGER, INTENT(in) :: ifms,ifme, jfms,jfme  ! fire memory indices

INTEGER, INTENT(in) :: itimestep         ! current time step (cumulative)
REAL,    INTENT(in) :: dt                ! time step
REAL,    INTENT(in) :: dx,dy             ! dx,dy on innermost atm mesh

!  ***** data (constant in time) *****
! height above sea level of w points (m)
REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: zs ! constant

REAL, INTENT(in), DIMENSION( ifms,ifme, jfms,jfme ):: &
           fgi !initial total mass of surface fuel (kg/m^2), at fire cells

!  ***** inputs *****
! incoming atmos. winds (m/s at arakawa-c grid locations)
REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: u,v

!  ***** outputs *****
real, intent(out) DIMENSION( ims:ime, jms:jme ) grndhx,grndqx

!  ***** state *****
! these are state vectors, to be used in data assimilation 
real, intent(inout), dimension(ifms,ifme, jfms,jfme):: &
            phi,        & ! the level function, node based, fire is where phi<0
            tign,       & ! ignition time, node based
            fuel_frac     ! fuel fraction, cell based (redundant)
            
!
            
            
!*** local

integer::iats,iate,jats,jate         ! atm grid size
! fire grid size declared in module_fr_sfire_speed
real, dimension(ifms:ifme, jfms:jfme):: & 
    phi_start,      & ! the level function at start (of this call)
    fuel_time,      & ! time for fuel to burn down to 1/e original amount (data)
    fuel_frac_start,& ! the fuel fraction at start
    zf,zsf            ! terrain height
    
! kw defined in module_fr_sfire_speed
real, dimension(ifms:ifme, jfms:jfme,1:kw):: vfx,vfy
real, target, dimension(ifms:ifme, jfms:jfme):: &  ! arrays for the speed function
    t_vx,t_vy,     &   ! wind 
    t_dzfsdx,t_dzfsdy  ! terrain gradient

real, dimension(ims:ime, jms:jme)::grnhft,grnqft
real:: fdx,fdy,time_start,dmass

!*** executable

! pass local automatic arrays to the speed function in module_fr_sfire_speed
vx => t_vx
vy => t_vy
dzfsdx => t_dzfsdx
dzfsdy => t_dzfsdy

! time - assume dt does not change
time_start = itimestep * dt

! mesh step
fdx=dx/nfrx
fdy=dy/nfrx

! init rest of fuel tables with derived quantities
call init_fuel_cats

! get the index range of atm cells over the fire domain
! leave out one cell strip at domain boundary
iats = MAX( its, ids+1)
iate = MIN( ite, ide-1)
jats = MAX( jts, jds+1)
jate = MIN( jte, jde-1)

! get the index range of the matching cells on the refined fire grid
ifts = (iats-1)* nfrx + 1
ifte = iate * nfrx
jfts = (jats-1)* nfry + 1
jfte = jate * nfry

write(msg,7001)'atm domain ','ids',ids,ide,jds,jde
call message(msg)                    
write(msg,7001)'atm memory ','ims',ims,ime,jms,jme
call message(msg)                    
write(msg,7001)'atm patch  ','its',its,ite,jts,jte
call message(msg)                    
write(msg,7001)'fire domain','ifds',ifds,ifde,jfds,jfde
call message(msg)                    
write(msg,7001)'fire memory','ifms',ifms,ifme,jfms,jfme
call message(msg)                    
write(msg,7001)'atm used   ','iats',iats,iate,jats,jate
call message(msg)                   
write(msg,7001)'fire used  ','ifts',ifts,ifte,jfts,jfte
call message(msg)                    
7001 format(a,' dimensions ',a4,':',i8,' to ',i8,' by ',i8,' to ',i8)
write(msg,'(a,2i9)')'refinement ratio:',nfrx,nfry
call message(msg)
write(msg,'(a,2f15.6)')'atmosphere mesh step:',dx,dy
call message(msg)
write(msg,'(a,2f15.6)')'fire mesh step:      ',fdx,fdy
call message(msg)

! check if nodal mesh fits into the declared memory 
call check_mesh_2dim(iats,iate+1,jats,jate+1,ims,ime,jms,jme)
call check_mesh_2dim(ifts,ifte+1,jfts,jfte+1,imfs,imfe,jmfs,jmfe)

! interpolate winds from u,v on the 3d arakawa c-grid (midpoints of faces)
! to vfx,vfy on nodes of the fire grid, in layers 1 to kw
! kw, vx,vy is declared in module_fr_sfire_speed
! *** this might be optimized away and the advection field computed from
! the atm c-grid points directly ***
call atm2fire_winds(                      &
    iats,iate,jats,jate,                &
    ims,ime, kms,kme,jms,jme,           & 
    ifts,ifte,jfts,jfte,1,kw,           &
    ifms,ifme,jfms,jfme,1,kw,           &
    u,v,vfx,vfy)

!interpolate topography from cell centers on atm grid to nodes on fire grid (zs -> zsf)
call interpolate_2d_cells2nodes(              &
    iats,iate,jats,jate, &
    ims, ime, jms, jme, zs, &  ! in  
    ifts,ifte,jfts,jfte, &
    ifms,ifme,jfms,jfme,zsf  )  ! out

!***** set up arrays to be passed to the speed function
! We cannot simply compute the spread rate here because that will change with the
! angle of the wind and the direction of propagation, thus it is done in subroutine
! normal_spread at each fire time step. Instead, we set some quantities that 
! subroutine normal_spread may use.
do j=jfts,jfte+1
    do i=ifts,ifte+1
        vx(i,j)=vfx(i,j,1)
        vy(i,j)=vfy(i,j,1)
        dzfsdx(i,j)=0.5*(zsf(i+1,j)-zsf(i-1,j))/fdx
        dzfsdy(i,j)=0.5*(zsf(i,j+1)-zsf(i,j-1))/fdy
    enddo
enddo



do j=jfts,jfte
    do i=ifts,ifte

! --- get integer height of fuel
      ks = nint(1. + fueldepthm(nfuel_cat(i,j)) / (zf(i,j)-zsf(i,j)))
    enddo
enddo

!???????????
      
! advance the model from time_start to time_start+dt
! return the fuel fraction burnt this call in each fire cell
! will call module_fr_sfire_speed%%normal_speed for propagation speed
! uses vx,vy,r as a background propagation field 
call sfire_core(     &
    ifts,ifte,jfts,jfte,                      &
    ifms,ifme,jfms,jfme,                      &
    time_start,time_diff,fdx,fdy,fuel_time,   &
    phi,tign,fuel_frac,fuel_frac_burnt ) 

! compute the heat fluxes on the fire grid cells
do j=jfts,jfte
    do i=ifts,ifte
         dmass =                     &     ! ground fuel mass burnt this call (kg/m^2)
             fgi(nfuel_cat(i,j))     &     ! init mass from fuel model no (kg/m^2)
             * fuel_frac_burnt(i,j)  &     ! fraction burned this call    (1)
             / (nfrx*nfry)                 ! scale so that sum over atm cell = average
         grnhft(i,j) = (dmass/dt)*(1.-bmst)*cmbcnst         ! J/m^2/sec
         grnqft(i,j) = (bmst+(1.-bmst)*.56)*(dmass/dt)*xlv  ! what the #!@* ????
         ! xlv is defined in module_model_constants
    enddo
enddo

! sum up the fluxes over atm cells
call sum_2d_cells(                                  &
    ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,grnhft, &
    iats,iate,jats,jate,ims, ime, jms, jme, grnht)
call sum_2d_cells(                                  &
    ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,grnqft, &
    iats,iate,jats,jate,ims, ime, jms, jme, grnqt)

! --- add heat and moisture fluxes to tendency variables

CALL fire_tendency(grnhfx,grnqfx,canhfx,canqfx,    &  ! send
                      alfg,alfc,z1can,                &
                      zs,z_at_w,dz8w,mu,rho,          &
                      ids,ide, kds,kde, jds,jde,      &
                      ims,ime, kms,kme, jms,jme,      &
                      its,ite, kts,kte, jts,jte,      &
                      rthfrten,rqvfrten)                 ! recv


end subroutine sfire_model

!
!*****************
!

SUBROUTINE fire_tendency(grnhfx,grnqfx,canhfx,canqfx, &  ! incoming
                         alfg,alfc,z1can,             &
                         zs,z_at_w,dz8w,mu,rho,       &
                         ids,ide, kds,kde, jds,jde,   &
                         ims,ime, kms,kme, jms,jme,   &
                         its,ite, kts,kte, jts,jte,   &
                         rthfrten,rqvfrten)              ! outgoing

! --- this routine takes fire generated heat and moisture fluxes and
!     calculates their influence on the theta and water vapor 

! --- note that these tendencies are valid at the Arakawa-A location

   IMPLICIT NONE

! --- incoming variables

   INTEGER , INTENT(in) :: ids,ide, kds,kde, jds,jde, &
                           ims,ime, kms,kme, jms,jme, &
                           its,ite, kts,kte, jts,jte

   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: grnhfx,grnqfx  ! W/m^2
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: canhfx,canqfx  ! W/m^2
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: zs  ! topography (m abv sealvl)
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: mu  ! dry air mass (Pa)

   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: z_at_w ! m abv sealvl
   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: dz8w   ! dz across w-lvl
   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: rho    ! density

   REAL, INTENT(in) :: alfg ! extinction depth ground fire heat (m)
   REAL, INTENT(in) :: alfc ! extinction depth crown  fire heat (m)
   REAL, INTENT(in) :: z1can    ! height of crown fire heat release (m)

! --- outgoing variables

   REAL, INTENT(out), DIMENSION( ims:ime,kms:kme,jms:jme ) ::   &
       rthfrten, & ! theta tendency from fire (in mass units)
       rqvfrten    ! Qv tendency from fire (in mass units)
! --- local variables

   INTEGER :: i,j,k
   INTEGER :: i_st,i_en, j_st,j_en, k_st,k_en

   REAL :: cp_i
   REAL :: rho_i
   REAL :: xlv_i
   REAL :: z_w,dz
   REAL :: fact_g, fact_c

   REAL, DIMENSION( ims:ime,kms:kme,jms:jme ) :: hfx,qfx

! --- set some local constants

   cp_i = 1./cp     ! inverse of specific heat
   xlv_i = 1./xlv   ! inverse of latent heat

! --- set loop indicies : note that 

   i_st = MAX(its,ids+1)
   i_en = MIN(ite,ide-1)
   k_st = kts
   k_en = MIN(kte,kde-1)
   j_st = MAX(jts,jds+1)
   j_en = MIN(jte,jde-1)

! --- distribute fluxes

   DO j = j_st,j_en
      DO k = k_st,k_en
         DO i = i_st,i_en

            ! --- set z (in meters above ground)

            z_w = z_at_w(i,k,j) - zs(i,j) ! should be zero when k=k_st

            ! --- heat flux

            fact_g = cp_i * EXP( - alfg * z_w )
            IF ( z_w < z1can ) THEN
               fact_c = cp_i
            ELSE
               fact_c = cp_i * EXP( - alfc * (z_w - z1can) )
            END IF
            hfx(i,k,j) = fact_g * grnhfx(i,j) + fact_c * canhfx(i,j) 

            ! --- vapor flux

            fact_g = xlv_i * EXP( - alfg * z_w )
            IF (z_w < z1can) THEN
               fact_c = xlv_i
            ELSE
               fact_c = xlv_i * EXP( - alfc * (z_w - z1can) )
            END IF
            qfx(i,k,j) = fact_g * grnqfx(i,j) + fact_c * canqfx(i,j) 

         END DO
      END DO
   END DO

! --- add flux divergence to tendencies
!
!   multiply by dry air mass (mu) to eliminate the need to 
!   call sr. calculate_phy_tend (in dyn_em/module_em.F)

   DO j = j_st,j_en
      DO k = k_st,k_en-1
         DO i = i_st,i_en

            rho_i = 1./rho(i,k,j)

            rthfrten(i,k,j) = - mu(i,j) * rho_i * (hfx(i,k+1,j)-hfx(i,k,j)) / dz8w(i,k,j)
            rqvfrten(i,k,j) = - mu(i,j) * rho_i * (qfx(i,k+1,j)-qfx(i,k,j)) / dz8w(i,k,j)

         END DO
      END DO
   END DO

   RETURN

END SUBROUTINE fire_tendency

end module module_fr_sfire_model
