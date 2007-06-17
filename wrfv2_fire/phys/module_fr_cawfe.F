#define _FR_DBG_LEVEL_ 100

! =============================================================================
!
!  This set of modules contains the fire code ported from the CAWFE model.
!
!      http://www.mmm.ucar.edu/research/wildfire/afm/afm.html
!
!  This version is no longer compatible with the Clark-Hall model.  It is 
!  now WRF specific.
!
!  Created by:         Edward (Ned) G. Patton
!                      National Center for Atmospheric Research
!                      Mesoscale and Microscale Meteorology Division
!                      Boulder, Colorado 80307-3000
!                      patton@ucar.edu
!
!  Under guidance by:  Janice L. Coen
!                      National Center for Atmospheric Research
!                      Mesoscale and Microscale Meteorology Division
!                      Boulder, Colorado 80307-3000
!                      coen@ucar.edu
! 
!  Based on:           Clark, T. L., J. L. Coen and D. Latham: 2004, 
!                      "Description of a coupled atmosphere-fire model",
!                      International Journal of Wildland Fire, 13, 49-63.
!
!  See below for a description of the variables.
!
!  There are two modules:  1) module_fr_cawfe_fuel  (sets fuel params)
!                          2) module_fr_cawfe       (the fire code)
! 
! =============================================================================

MODULE module_fr_cawfe_fuel

   INTEGER, PARAMETER :: nfuelcats = 14

   INTEGER, DIMENSION( nfuelcats ) :: ichap
   REAL   , DIMENSION( nfuelcats ) :: weight,fgi,fci,fci_d,fct,fcbr, &
                                            fueldepthm,fueldens,fuelmce,   &
                                            savr,st,se

! =============================================================================
! ----- Specification of fuel properties for the standard 13 fire 
!          behavior fuel models (for surface fires), along with some
!          estimated canopy properties (for crown fire).
! =============================================================================
!  FUEL MODEL 1: Short grass (1 ft)
!  FUEL MODEL 2: Timber (grass and understory)
!  FUEL MODEL 3: Tall grass (2.5 ft)
!  FUEL MODEL 4: Chaparral (6 ft)
!  FUEL MODEL 5: Brush (2 ft) 
!  FUEL MODEL 6: Dormant brush, hardwood slash
!  FUEL MODEL 7: Southern rough
!  FUEL MODEL 8: Closed timber litter
!  FUEL MODEL 9: Hardwood litter
!  FUEL MODEL 10: Timber (litter + understory)
!  FUEL MODEL 11: Light logging slash
!  FUEL MODEL 12: Medium logging slash
!  FUEL MODEL 13: Heavy logging slash
!
   DATA fgi / 0.166, 0.897, 0.675, 2.468, 0.785, 1.345, 1.092, &
              1.121, 0.780, 2.694, 2.582, 7.749, 13.024, 1.e-7 /
   DATA fueldepthm /0.305,  0.305,  0.762, 1.829, 0.61,  0.762,0.762, &
                    0.0610, 0.0610, 0.305, 0.305, 0.701, 0.914, 0.305 /
   DATA savr / 3500., 2784., 1500., 1739., 1683., 1564., 1562.,  &
               1889., 2484., 1764., 1182., 1145., 1159., 3500. /
   DATA fuelmce / 0.12, 0.15, 0.25, 0.20, 0.20, 0.25, 0.40,  &
                  0.30, 0.25, 0.25, 0.15, 0.20, 0.25, 0.12  / 
   DATA fueldens / nfuelcats * 32. /   ! 32 if solid, 19 if rotten.
   DATA st / nfuelcats* 0.0555 /
   DATA se / nfuelcats* 0.010 /

! ----- Notes on weight: (4) - best fit of Latham data;
!                 (5)-(7) could be 60-120; (8)-(10) could be 300-1600;
!                 (11)-(13) could be 300-1600
   DATA weight / 7.,  7.,  7., 180., 100., 100., 100.,  &
              900., 900., 900., 900., 900., 900., 7. / 

! ----- 1.12083 is 5 tons/acre.  5-50 tons/acre orig., 100-300 after blowdown
   DATA fci_d / 0., 0., 0., 1.123, 0., 0., 0.,  &
            1.121, 1.121, 1.121, 1.121, 1.121, 1.121, 0./
   DATA fct / 60., 60., 60., 60., 60., 60., 60.,  &
            60., 120., 180., 180., 180., 180. , 60.    /
   DATA ichap / 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 / 

! =========================================================================

CONTAINS

   SUBROUTINE init_module_fire_fuel
   END SUBROUTINE init_module_fire_fuel

END MODULE module_fr_cawfe_fuel


! =========================================================================
!
!  The following module contains the fire code ported from the Clark-Hall model.
!
! =========================================================================


MODULE module_fr_cawfe

   USE module_model_constants

   INTEGER, SAVE :: igwiz

   REAL, PARAMETER :: cmbcnst  = 17.433e+06             ! J/kg dry fuel
   REAL, PARAMETER :: hfgl     = 17.e4                  ! W/m^2
   REAL, PARAMETER :: fuelheat = cmbcnst * 4.30e-04     ! convert J/kg to BTU/lb
   REAL, PARAMETER :: fuelmc_g = 0.08                   ! set = 0 for dry ground fuel
   REAL, PARAMETER :: fuelmc_c = 1.00                   ! set = 0 for dry canopy
   REAL, PARAMETER :: bmst     = fuelmc_g/(1+fuelmc_g)
!  REAL, PARAMETER :: ep       = epsilon                ! epsilon a very small number
!  REAL, PARAMETER :: ep_sq    = epsilon**2
   REAL, PARAMETER :: ep       = 1.e-7                  ! epsilon a very small number
   REAL, PARAMETER :: ep_sq    = ep**2

   REAL :: tignm
   REAL :: dxf,dyf
   REAL :: betaop,c,e
   REAL :: grndhx,grndqx,canhx,canqx

   REAL, DIMENSION( 4 ) :: xlm,ylm

   DATA igwiz /0/   ! igwiz=0 1st time through firecode at startup,restart

CONTAINS

! =========================================================================

SUBROUTINE cawfe(ids,ide, kds,kde, jds,jde,                    & ! incoming
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
  
! -------------------------------------------------------------------------
!
! ---- key indicies
!
!  ids,ide        start,end domain indicies for atmos. variables in x
!  kds,kde        start,end domain indicies for atmos. variables in z
!  jds,jde        start,end domain indicies for atmos. variables in y
!  ims,ime        start,end memory dimensions for atmos. variables in x
!  kms,kme        start,end memory dimensions for atmos. variables in z
!  jms,jme        start,end memory dimensions for atmos. variables in y
!  its,ite        start,end tile   indicies for atmos. variables in x
!  kts,kte        start,end tile   indicies for atmos. variables in z
!  jts,jte        start,end tile   indicies for atmos. variables in y
!
!  ifds,ifde      start,end domain dimensions for fire variables indicies in x
!  jfds,jfde      start,end domain dimensions for fire variables indicies in y
!  ifms,ifme      start,end memory dimensions for fire variables indicies in x
!  jfms,jfme      start,end memory dimensions for fire variables indicies in y
!
!  if_st,if_en    start,end tile indicies for fire variables indicies in x
!  jf_st,jf_en    start,end tile indicies for fire variables indicies in y
!
! ---- incoming WRF variables
!
!  itimestep      integer time step 
!  dt             time increment of atmos. grid that contains the fire grid
!  dx,dy          delta x,y of innermost atmos. grid that contains the fire grid
!  grid_id        integer grid id pointing to the WRF domain which are we on
!  cen_lat        latitude of the center of the fire domain
!  cen_lon        longitude of the center of the fire domain
!  lat_ll         latitude of the south-west corner of the fire domain
!  lon_ll         longitude of the south-west corner of the fire domain
!  moad_cen_lat   latitude of the center of the mother of all domains (moad)
!  moad_cen_lon   longitude of the center of the moad
!  moad_lat_ll    latitude of the south-west corner of the moad
!  moad_lon_ll    longitude of the south-west corner of the moad
!  moad_dx        delta x of moad grid
!  moad_dy        delta y of moad grid
!  moad_s_we      starting west-east index of moad grid
!  moad_e_we      ending west-east index of moad grid
!  moad_s_sn      starting south-north index of moad grid
!  moad_e_sn      ending south-north index of moad grid
!
!  nfrx,nfry      specified innermost atmos. grid refinement for fire grid in x,y
!  tlat_stf       latitude of inital fire location (degrees lat)
!  tlon_stf       longitude of inital fire location (degrees lon)
!  t_ignite       time of fire initiation (s)
!  ishape         what initial fire shape? 
!  ibeh           which fire spread model?  = 0 McArthur, =1 BEHAVE
!  z1can          lowest height crown fire heat is released (m)
!  alfc           extinction depth of crown fire heat (m)
!  alfg           extinction depth of ground fire heat (m)
!  ifuelread      read fuel parameters from file? or specify them?
!  nfuel_cat0     if fuel parameters are specified, what category?
!
!  z              height above sea level of mass points (m)
!  z_at_w         height above sea level of w points (m)
!  dz8w           delta z between w points (m)
!  zs             height of surface above sea level (m)
!  u,v            incoming atmos. winds (m/s at arakawa-c grid locations)
!  mu            
!  rho            incoming atmos. density (kg/m^3 at arakawa-c grid mass points)
!
!
! ---- in and out fire variables
!
!  nfuel_cat(i,j) integer NFFL fuel category at each fire grid cell
! jm data, read at initialization, then IN only

! jm part of tracer data structure
!  nfl(i,j) = 0   means no fire line points in that cell
!  nfl(i,j) = 1   means there are some fire line points in that cell
!  ncod(i,j,it)   is number of fire line coordinates in fire grid (i,j)
!  in1(i,j,1)     is i1 index for (i,j) grid
!  in1(i,j,2)     is j1 index for (i,j) grid
!  in2(i,j,1)     is i2 index for (i,j) grid
!  in2(i,j,2)     is j2 index for (i,j) grid
!  ixb(i,j,it)    = 0 means the x coord of the it tracer is within ep of boundary
!  ixb(i,j,it)    = 1 means the x coord of the it tracer is an interior point
!  iyb(i,j,it)    = 0 means the y coord of the it tracer is within ep of boundary
!  iyb(i,j,it)    = 1 means the y coord of the it tracer is an interior point
!  icn(i,j,it)    = 0 means the it coord is not a corner point
!  icn(i,j,it)    = 1 means the it coord is a corner point
! jm end of tracer data structure it=1:4

!  fg(i,j)        mass of surface fuel (kg/m^2)
! jm mass of fuel remaning

!  fc(i,j)        total mass of canopy fuel  (kg/m^2)
! jm mass of fuel remaining

!  r_0(i,j)       is the spread rate for a fire on flat ground with no wind
! jm something like diffusion coefficient; here does not change with time

!  bbb(i,j)       is a constant in the wind correction for fire spread
!  betafl(i,j)    is a constant in the wind correction for fire spread
!  phiwc(i,j)     is a constant in the wind correction for fire spread
!  area(i,j)      
! jm relative area of cell burning, from 0 to 1, stays at 1 after burned out

!  area2(i,j)     the sum over this is the fire area (m^2)
! jm about the same thing, used for calculation, have to store it somewhere

!  zf(i,j)        hgt of kds+1 mass point above sea level interp. to fire grid (m)
! jm topography, vertical index to range from kds to kde

!  zsf(i,j)       hgt of surface above sea level interpolated to fire grid (m)
! jm topography

!  tign_g(i,j)    time this cell ignited ground fire (s); < 0 -> no ignition
!  tign_c(i,j)    time this cell ignited crown fire (s); < 0 -> no ignition
!  tign_crt(i,j)  time this cell ignited completely (s); < 0 -> not entirely aflame
!  xfg(i,j,4)     x coord of the 4 surface fuel tracers specific to grid (i,j)
!  yfg(i,j,4)     y coord of the 4 surface fuel tracers specific to grid (i,j)
!  xcd(i,j,it)    x coord of the it fire line coordinate in grid (i,j)
!  ycd(i,j,it)    y coord of the it fire line coordinate in grid (i,j)
!  xcn(i,j,it)    x coord of the normal vector at point (i,j)
!  ycn(i,j,it)    y coord of the normal vector at point (i,j)
! jm tracer things

!  sprdx(i,j,it)  spread rate in x normalized units (mostly for debug)
!  sprdy(i,j,it)  spread rate in y normalized units (mostly for debug)
! jm ???
!
! ---- key output variables from fire
!
!  rthfrten       theta tendency due to fire induced heat flux divergence
! jm  3D, potential temperature indexed by atmospheric grid

!  rqvfrten       Qv tendency due to fire induced moisture flux divergence
! jm  water vapor mixing ratio = mass of water vapor/unit mass of dry air

! ---- local variables - interface of atmosphere and fire

!  grnhfx         heat flux from ground fire (W/m^2) 
!  grnqfx         moisture flux from ground fire (W/m^2) 
!  canhfx         heat flux from crown fire (W/m^2) 
!  canqfx         moisture flux from crown fire (W/m^2) 
! jm 2d, specified at surface
!
! ---- local fire variables
!
!  fgi            initial total mass of surface fuel (kg/m^2)
! jm in this code constant everywhere, should be from a fuels

!  fci            initial total mass of crown fuel
!  fci_d          initial dry mass of crown fuel
!  fuelmc_c       initial moisture/dry mass ratio for crown fuel
! jm the ground fuel was done in a different way

!  fct            burn out time for crown fuel, after dry (s)
! jm in ground fire, "weight" plays similar role 

!  fcbr           crown fuel burn rate (kg/m^2/s)
! jm calculated

!  cmbcnst        joules per kg of dry fuel
! jm data

!  fuelmc_g       fuel particle (surface) moisture content
!                 fuel moisture fuelmc_g = (h2o)/(dry) 
! jm water contents in surface fuels

!  bmst           ratio of latent to sensible heat from sfc burn: 
!                   % of total fuel mass that is water (not quite 
!                   = % fuel moisture).    bmst= (h2o)/(h2o+dry) 
!                   so bmst = fuelmc_g / (1 + fuelmc_g)  where 
!                   fuelmc_g = ground fuel moisture
! jm calculated

!  hfgl           surface fire heat flux threshold to ignite canopy (W/m^2)
! jm data

!  fuelloadm      ovendry fuel loading, kg/m^2 (converted to lb/ft^2)
! jm calculated?

!  fueldepthm     fuel depth, in m  (converted to ft)
! jm data

!  savr           fuel particle surface-area-to-volume ratio (1/ft)
!                    grass: 3500., 10 hr fuel: 109.,  100 hr fuel: 30.
! jm data

!  fuelheat       fuel particle low heat content (btu/lb)
! jm computed scalar  

!  fueldens       ovendry particle density (lb/ft^3)
! jm how packed, ought to be data array but now a scalar

!  st             fuel particle total mineral content
! jm data, scalar

!  se             fuel particle effective mineral content
! jm data, scalar

!  fuelmce        moisture content of extinction; 
!                    0.30 for many dead fuels; 0.15 for grass
! jm data, scalar

!  weight         weighting parameter determining the slope of the mass loss curve
!                    ranges from ~5 (fast burnup) to 1000 ( ~40% decr over 10 min).
! jm determines speed of burning of ground fuel, add-on to fuel type table

!  sfcu(i,j,6)    surface wind in x-direction interpolated to grid corners (m/s)
!  sfcv(i,j,6)    surface wind in y-direction interpolated to grid corners (m/s)
! jm calculated
! 
!    when deriving fireline coordinates we always keep the fire to our left
!
! -------------------------------------------------------------------------
!
!     .... Three components to this subroutine:
!
!        Initialization
!             IGWIZ loop: Initialization of variables for all 
!             runs including restart.  Executed once each time the 
!             model is started on 1st pass through fire_sfc .
!        Pass through tracer scheme.
!             This defines fire boundary and (ground) fire line 
!             progression in this time step. 
!        Calculation of fluxes to be fed into atmosphere.
!             Calculates macroscale properties of this 
!             fire line progression back on atmosphere in terms of
!             mass burned -> heat and vapor fluxes to be fed into atmos.
!             3 parts to this: 
!                 1) ground fire heat+vapor release.
!                 2) some ground fire heat used to dry overlying canopy.
!                 3) canopy fire ignition, heat+vapor release.
!
! -------------------------------------------------------------------------
!
!    - should sometime integrate fuel depth (veg type) with ZNOT?
!
!   Conversions:
!            Fuel loads:   1 ton/acre = 0.224166 kg/m^2
! -------------------------------------------------------------------------

   USE module_fr_cawfe_fuel
   USE module_wrf_error
!  USE module_tecplot

   IMPLICIT NONE

! ----- incoming variables

   INTEGER, INTENT(in) :: ids,ide, kds,kde, jds,jde    ! atmosphere domain indices
   INTEGER, INTENT(in) :: ims,ime, kms,kme, jms,jme    ! atmosphere memory indices
   INTEGER, INTENT(in) :: its,ite, kts,kte, jts,jte    ! atmosphere tile   indices
 
   INTEGER, INTENT(in) :: ifds,ifde, jfds,jfde, kfds,kfde  ! fire domain indices
   INTEGER, INTENT(in) :: ifms,ifme, jfms,jfme, kfms,kfme  ! fire memory indices

   INTEGER, INTENT(in) :: itimestep		    ! current time step (cumulative)
   REAL,    INTENT(in) :: dt			    ! time step
   REAL,    INTENT(in) :: dx,dy			    ! dx,dy on innermost atm mesh
   INTEGER, INTENT(in) :: grid_id		    ! grid id of innermost atm mesh
   REAL, INTENT(in)    :: cen_lat,cen_lon	    ! center lat,lon of " " "
   REAL, INTENT(in)    :: lat_ll,lon_ll		    ! lat,lon of sw corner of " " "
   REAL, INTENT(in)    :: moad_cen_lat,moad_cen_lon ! lat,lon of center of moad
   REAL, INTENT(in)    :: moad_lat_ll,moad_lon_ll   ! lat,lon of sw corner of moad
   REAL, INTENT(in)    :: moad_dx,moad_dy	    ! dx,dy of moad
   INTEGER, INTENT(in) :: moad_s_we,moad_e_we	    ! strt/stop grd pts in x moad
   INTEGER, INTENT(in) :: moad_s_sn,moad_e_sn       ! strt/stop grd pts in y moad

   INTEGER, INTENT(in) :: nfrx,nfry   ! innermost atmos. grid refinement for fire grid in x,y
   REAL, INTENT(in)    :: tlat_stf    ! latitude of inital fire location (degrees lat)
   REAL, INTENT(in)    :: tlon_stf    ! longitude of inital fire location (degrees lon) 
   REAL, INTENT(in)    :: t_ignite    ! time of fire initiation (s)
   INTEGER, INTENT(in) :: ifuelread   ! read fuel parameters from file? or specify them?
   INTEGER, INTENT(in) :: ishape      ! what initial fire shape? 
   REAL, INTENT(in)    :: z1can       ! lowest height crown fire heat is released (m)
   REAL, INTENT(in)    :: alfg        ! extinction depth of ground fire heat (m)
   REAL, INTENT(in)    :: alfc        !  extinction depth of crown fire heat (m)
   INTEGER, INTENT(in) :: ibeh        ! which fire spread model?  = 0 McArthur, =1 BEHAVE
   INTEGER, INTENT(in) :: nfuel_cat0  ! if fuel parameters are specified, what category?

   ! incoming atmos. winds (m/s at arakawa-c grid locations)
   REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: u,v
   ! height above sea level of w points (m)
   REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: z,z_at_w
   ! incoming atmos. density (kg/m^3 at arakawa-c grid mass points)
   REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: rho
   ! delta z between w points (m)
   REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: dz8w
   ! dry air mass (Pa)
   REAL, INTENT(in), DIMENSION( ims:ime, jms:jme )          :: mu
   ! topography (m abv sealvl)
   REAL, INTENT(in), DIMENSION( ims:ime, jms:jme )          :: zs

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

   REAL, INTENT(inout), DIMENSION( ims:ime,kms:kme,jms:jme ) :: rthfrten
   REAL, INTENT(inout), DIMENSION( ims:ime,kms:kme,jms:jme ) :: rqvfrten

! ----- out variables

!   REAL,  DIMENSION( ims:ime,jms:jme ) :: grnhfx,grnqfx
!   REAL,  DIMENSION( ims:ime,jms:jme ) :: canhfx,canqfx
   REAL, INTENT(out), DIMENSION( ims:ime,jms:jme ) :: grnhfx,grnqfx
   REAL, INTENT(out), DIMENSION( ims:ime,jms:jme ) :: canhfx,canqfx

! ----- local variables

   INTEGER :: if_st,if_en, jf_st,jf_en
   INTEGER :: nf
   INTEGER :: it
   INTEGER :: i,j
   INTEGER :: ib,jb
   INTEGER :: idts
   INTEGER :: iyes
   INTEGER :: ig,jg
   INTEGER :: ic,jc

   REAL :: time
   REAL :: delplot
   REAL :: rad
   REAL :: sumarea
   REAL :: a_fl
   REAL :: burn_frac
   REAL :: delm
   REAL :: bratio
   REAL :: dmass
   REAL :: fcav
   REAL :: delh
   REAL :: grnhsum,canhsum
   REAL :: ratg,ratc
   REAL :: teps
   REAL :: ratio

   REAL, DIMENSION( ims:ime,jms:jme,6 ) :: sfcu,sfcv

   REAL, DIMENSION( ifms:ifme,jfms:jfme ) :: grnhft,grnqft
   REAL, DIMENSION( ifms:ifme,jfms:jfme ) :: canhft,canqft

   CHARACTER (LEN=256) :: msg

! --- take incoming horizontal atmos. winds on arakawa-c grid and 
!     interpolate them to the cube corners.  resulting winds (sfcu,sfcv)
!     are at the south west corner of the atmos. cell and shifted up
!     one-half grid point to the w-level.   sfcu,sfcv will be further 
!     interpolated to the needed locations behind the fire line in
!     sr. fire_tr.

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
 
   IF (igwiz == 0) THEN         ! igwiz loop
 
      igwiz = 1

      dxf = dx / FLOAT(nfrx)
      dyf = dy / FLOAT(nfry)

      WRITE(msg,*)'fire: dx,dy, dxf,dyf=', dx,dy, dxf,dyf
      CALL wrf_message ( msg )

! ----- fuel moisture parameters

      DO nf = 1,nfuelcats
         fci(nf) = (1.+fuelmc_c)*fci_d(nf)
         fcbr(nf) = fci_d(nf)/fct(nf)
      END DO

! ----- initialize fire-related constants, and fuel category data

      call fire_topography( z,zs,                         &  ! incoming
                         ids,ide, kds,kde, jds,jde,            &
                         ims,ime, kms,kme, jms,jme,            &
                         its,ite, kts,kte, jts,jte,            &
                         ifms,ifme, kfms,kfme, jfms,jfme,      &
                         if_st,if_en,jf_st,jf_en,              &
                         nfrx,nfry,                            &
                         zf,zsf)                            ! outgoing
 
      CALL fire_startup( grid_id,                              & ! in
                         ifms,ifme, kfms,kfme, jfms,jfme,      &
                         if_st,if_en,jf_st,jf_en,              &
                         ifuelread, nfuel_cat0,                &
                         zf,zsf,                               &
                         nfuel_cat,betafl,bbb,r_0,phiwc ) ! out

! -- EGP converted data statements from blockdata.f (check this for restarts...)

      DO j = jf_st,jf_en
      DO i = if_st,if_en
         tign_g(i,j) = -100.
         tign_c(i,j) = -100.
         tign_crt(i,j) = -100.
         DO it = 1,4
            xfg(i,j,it) = 0.
            yfg(i,j,it) = 0.
         END DO
         nfl(i,j) = 0 
      END DO
      END DO

! ----- check if fire has been ignited (i.e. TIGNM > 0.)

      tignm = -100.
      DO j = jf_st,jf_en
      DO i = if_st,if_en
         tignm = MAX(tign_g(i,j),tign_c(i,j),tignm)
      END DO
      END DO

!!!! EGP for MPI we need to communicate tignm to all processors
 
! ----- if no fire has been ignited, initialize some fireline variables,
!            including fuel loads.

      IF (tignm < -10.) THEN                      

! ----- iof=1

          CALL fire_init(1,dt,                                     & ! send
                         cen_lat,cen_lon,lat_ll,lon_ll,            &
                         moad_cen_lat,moad_cen_lon,moad_dx,moad_dy,&
                         moad_s_we,moad_e_we,moad_s_sn,moad_e_sn,  &
                         nfl_t,time,dx,dy,nfuel_cat,               &
                         xcd,ycd,xcn,ycn,sprdx,sprdy,           &
                         ifds,ifde, kfds,kfde, jfds,jfde,          &
                         ifms,ifme, kfms,kfme, jfms,jfme,          &
                         if_st,if_en,jf_st,jf_en,                  &
                         nfrx,nfry,                                &
                         ishape,tlat_stf,tlon_stf,t_ignite,icn,    &
                         fg,fc,tign_g,nfl,xfg,yfg,                 & ! send&recv
                         ixb,iyb)                                    ! recv

      END IF                                      

   END IF     ! igwiz  loop

! ----- end initialization

! ----- t_ignite is time we want the fire ignited (in model time).
!
!     This call:
!        - ignites a fire if t_ignite-time/60. < dt/60. 
!           (i.e. if we are within 1 time step of designated ignition)
!        - sets tignm=now (a time > 0.).

   WRITE(msg,*)'tignm = ',tignm
   CALL wrf_message ( msg )

   WRITE(msg,*)'t_ignite = ',t_ignite
   CALL wrf_message ( msg )

   WRITE(msg,*)'time     = ',time
   CALL wrf_message ( msg )

   WRITE(msg,*)'diff     = ',t_ignite-time,dt
   CALL wrf_message ( msg )

   CALL fire_init(2,dt,                                         & ! send
                  cen_lat,cen_lon,lat_ll,lon_ll,                &
                  moad_cen_lat,moad_cen_lon,moad_dx,moad_dy,    &
                  moad_s_we,moad_e_we,moad_s_sn,moad_e_sn,      &
                  nfl_t,time,dx,dy,nfuel_cat,                   &
                  xcd,ycd,xcn,ycn,sprdx,sprdy,               &
                  ifds,ifde, kfds,kfde, jfds,jfde,              &
                  ifms,ifme, kfms,kfme, jfms,jfme,              &
                  if_st,if_en,jf_st,jf_en,                      &
                  nfrx,nfry,                                    &
                  ishape,tlat_stf,tlon_stf,t_ignite,icn,        &
                  fg,fc,tign_g,nfl,xfg,yfg,                     & ! send&recv
                  ixb,iyb)                                        ! recv


   IF (tignm < -10.) RETURN  ! if nothing has ignited yet, return.
 
! ----- begin pass through tracer scheme
 
   DO jb = jts,MIN(jte,jde-1)
   DO ib = its,MIN(ite,ide-1)
      grnhfx(ib,jb) = 0.0
      grnqfx(ib,jb) = 0.0
      canhfx(ib,jb) = 0.0
      canqfx(ib,jb) = 0.0
   END DO
   END DO

   DO j = jf_st,jf_en
   DO i = if_st,if_en
      grnhft(i,j) = 0.0
      grnqft(i,j) = 0.0
      canqft(i,j) = 0.0
      canhft(i,j) = 0.0
   END DO
   END DO

! all sort of fixes to the fireline variables

   CALL fire_stat(1,dt,time,                       & ! send
                  xcd,ycd,xcn,ycn,sprdx,sprdy,  &
                  ifms,ifme, kfms,kfme, jfms,jfme, &
                  if_st,if_en,jf_st,jf_en,         &
                  nfl,nfl_t,tign_g,tign_crt,       & ! send&recv
                  area,area2,xfg,yfg,              &
                  ixb,iyb,icn)                       ! recv

! EGP --- temporarily dump fire line for tecplot

!     CALL fire_write_fire_ln(itimestep,                        &
!                             nfl,icn,xfg,yfg,dxf,dyf,          &
!                             ifms,ifme, kfms,kfme, jfms,jfme,  &
!                             if_st,if_en, jf_st,jf_en)

! EGP ---

! ----- create points outlining fire

   CALL fire_ln(dt,time,sprdx,sprdy,               & ! in
                ifms,ifme, kfms,kfme, jfms,jfme,      &
                if_st,if_en,jf_st,jf_en,              &
                ncod,in1,in2,ixb,iyb,icn,             & ! inout
                tign_g,tign_crt,area,area2,xfg,yfg,   &
                nfl,nfl_t,rad,xcd,ycd,xcn,ycn)          ! int

! ----- advect fire line points using spread rate

   CALL fire_tr(dt,ibeh,nfuel_cat,ncod,nfl,zf,zsf,zs,        &  ! send
                sfcu,sfcv,xcd,ycd,bbb,phiwc,betafl,r_0,      &
                ids,ide, kds,kde, jds,jde,                   &
                ims,ime, kms,kme, jms,jme,                   &
                its,ite, kts,kte, jts,jte,                   &
                ifms,ifme, kfms,kfme, jfms,jfme,             &
                if_st,if_en,jf_st,jf_en,                     &
                nfrx,nfry,                                   &
                xcn,ycn,                                     &  ! send&recv
                sprdx,sprdy)                                    ! recv

! ----- reset coordinates for fire line grids

   CALL fire_ds(ixb,iyb,icn,nfl,ncod,in1,in2,      & ! send
                time,xcn,ycn,xcd,ycd,              &
                ifms,ifme, kfms,kfme, jfms,jfme,   &
                if_st,if_en,jf_st,jf_en,           &
                xfg,yfg,tign_g)                      ! send&recv

! ----- identify and treat newly ignited cells

   CALL fire_igs(ixb,iyb,icn,in1,in2,ncod,time,    & ! send
                 xcn,ycn,xcd,ycd,sprdx,sprdy,   &
                 ifms,ifme, kfms,kfme, jfms,jfme,  &
                 if_st,if_en,jf_st,jf_en,          &
                 nfl,tign_g,xfg,yfg)                           ! send&recv

! ----- all sort of fixes to the fireline variables


   CALL fire_stat(2,dt,time,                       & ! send
                  xcd,ycd,xcn,ycn,sprdx,sprdy,  &
                  ifms,ifme, kfms,kfme, jfms,jfme, &
                  if_st,if_en,jf_st,jf_en,         &
                  nfl,nfl_t,tign_g,tign_crt,       & ! send&recv
                  area,area2,xfg,yfg,              &
                  ixb,iyb,icn)                       ! recv
    
! ----- end pass through tracer scheme 
! ----- now begin calculation of flux feedback to atmosphere
 
   sumarea = 0.

   DO 95 j = jf_st+1,jf_en-1

      jb = INT(FLOAT((j-2)/nfry+2)+ep)

      DO 96 i = if_st+1,if_en-1   !start of I,J fuel grid physics

         ib = INT(FLOAT((i-2)/nfrx+2)+ep)

! ----- ground fuel

         IF (fg(i,j) < ep) GO TO 97       !grid point is burned out
         IF (tign_g(i,j) < ep) GO TO 97   !grid point has not ignited !error
 
! ----- calc. change in burning area

         a_fl = area2(i,j)
         sumarea = sumarea + area2(i,j)
 
! ----- calc. mass burned based on time since ignition from mass loss curve

         CALL fire_burn_fcn(i,j,                              & ! send
                            nfuel_cat,nfl,ncod,in1,in2,       &
                            ixb,iyb,icn,time,area2,           &
                            tign_g,tign_crt,                  &
                            xcd,ycd,xcn,ycn,xfg,yfg,          &
                            ifms,ifme, kfms,kfme, jfms,jfme,  &
                            if_st,if_en,jf_st,jf_en,          &
                            burn_frac)                          ! recv

         delm = MAX(fg(i,j) - (1.-burn_frac)*fgi(nfuel_cat(i,j)),0.0) 
 
! ----- all ground fuel burns off at the same rate
!       add random component to burn rate for excitation

         bratio = 1.0
         dmass = delm
         fg(i,j) = MAX(fg(i,j)-dmass,0.0)

         grnhft(i,j) = dmass/dt*(1.-bmst)*cmbcnst         ! J/m^2/sec
         grnqft(i,j) = (bmst+(1.-bmst)*.56)*dmass/dt*xlv
 
! ----- drying out canopy

         fcav = fc(i,j)-(1.-a_fl)*fci(nfuel_cat(i,j))  &
                          -a_fl*fci_d(nfuel_cat(i,j))     ! canopy moist. to be dried

         IF (fcav > 0.0) THEN
            delh = MIN(grnhft(i,j)*dt,fcav*xlv)           ! J/m^2
            grnhft(i,j) = grnhft(i,j) - delh/dt           ! J/m^2/sec
            fc(i,j) = fc(i,j) - delh/xlv
            fcav = fcav - delh/xlv
            canqft(i,j) = canqft(i,j) + delh/dt
         END IF

! ----- ignition of canopy follows
!       if surface fire heat flux over threshold, has not yet ignited,
                          
         IF (grnhft(i,j) > hfgl .AND. tign_c(i,j) < -10. &
                                   .AND. fcav < ep) tign_c(i,j) = time

97       CONTINUE         !end of ground fire physics
 
! ----- canopy
 
         IF (tign_c(i,j) < -10.) GO TO 98  !grid point has not yet been ignited

         a_fl = .5*((xfg(i,j,4)-xfg(i,j,1))*(yfg(i,j,3)-yfg(i,j,2))   &
                   +(yfg(i,j,4)-yfg(i,j,1))*(xfg(i,j,2)-xfg(i,j,3)))
 
         IF (fc(i,j) < ep) GO TO 98        !canopy grid point is burned out
 
! ----- define mass available to burn

         delm = MAX(fc(i,j)-(1.-a_fl)*fci_d(nfuel_cat(i,j)),0.0)
         dmass = MIN(a_fl*fcbr(nfuel_cat(i,j))*dt,bratio*delm)

         fc(i,j) = fc(i,j) - dmass

         canhft(i,j) = dmass / dt * cmbcnst
         canqft(i,j) = .56 * dmass / dt * xlv
 
98       CONTINUE         !end of canopy fire physics

! ----- ib,jb are atm grid pt counters; i,j grid pt counters
!             grnhft, grnhfx are in  J/m^2/s (W/m^2)

         canhfx(ib,jb) = canhfx(ib,jb) + canhft(i,j)/FLOAT(nfrx*nfry)
         canqfx(ib,jb) = canqfx(ib,jb) + canqft(i,j)/FLOAT(nfrx*nfry)
         grnhfx(ib,jb) = grnhfx(ib,jb) + grnhft(i,j)/FLOAT(nfrx*nfry)
         grnqfx(ib,jb) = grnqfx(ib,jb) + grnqft(i,j)/FLOAT(nfrx*nfry)

96    END DO
95 END DO      ! end of i,j fuel grid physics

!--- EGP need to handle MPI issues with sumarea

   sumarea = sumarea * dxf * dyf

   WRITE(msg,23)'time (min)=',time/60.,' AREA (acre)=',sumarea/4047.
23 FORMAT (1x,a11,f10.4,3x,a13,f15.5)
   CALL wrf_message ( msg )

! ----- end calculation of flux feedback to atmosphere
 
! ----- generate some statistics for printout and future plotting

   grndhx = 0.0
   canhx = 0.0
   grnhsum = 0.0
   canhsum = 0.0
   grndqx = 0.0
   canhx = 0.0
   canqx = 0.0

!  DO j = jts,jte
!  DO i = its,ite

   DO j = MAX(jts,jds+1),MIN(jte,jde-1)
   DO i = MAX(its,ids+1),MIN(ite,ide-1)
      grndhx = MAX(grndhx,grnhfx(i,j))
      canhx = MAX(canhx,canhfx(i,j))
      grnhsum = grnhsum + grnhfx(i,j)*dx*dy
      canhsum = canhsum + canhfx(i,j)*dx*dy
      grndqx = MAX(grndqx,grnqfx(i,j))
      canqx = MAX(canqx,canqfx(i,j))
   END DO
   END DO

   WRITE(msg,93)grndhx,grndqx,canhx,canqx
93 FORMAT(1x,'GRNDHX=',e11.4,' GRNDQX=',e11.4,' CANHX=',e11.4,' CANQX=',e11.4)
   CALL wrf_message( msg )

   WRITE(msg,68)grnhsum,canhsum
68 FORMAT(1x,'TOTAL FIRE SENS-HEAT FLUXES GRNH/CANH = ',2e11.4,' WATTS')
   CALL wrf_message( msg )
   
! ----- end of statistics print out
 
   ig = 0
   jg = 0
   ratg = 0.0
   ic = 0
   jc = 0
   ratc = 0.0
   teps = 1.

   DO j = MAX(jts,jds+1),MIN(jte,jde-1)
   DO i = MAX(its,ids+1),MIN(ite,ide-1)
      ratio = grnqfx(i,j)/(teps+grnhfx(i,j))
      IF (ratio > ratg) THEN
         ig = i
         jg = j
         ratg = ratio
      END IF
      ratio = canqfx(i,j)/(teps+canhfx(i,j))
      IF (ratio > ratc) THEN
         ic = i
         jc = j
         ratc = ratio
      END IF
   END DO
   END DO

   IF (ratg > 1. .OR. ratc > 1.) THEN
      WRITE(msg,441)ratg,ig,jg,ratc,ic,jc,     &
         grnhfx(ig,jg),grnqfx(ig,jg),canhfx(ic,jc),canqfx(ic,jc)
441   FORMAT(1x,'RATG(I,J)=',e11.4,2i3,' RATC(I,J)=',e11.4,2i3, &
         'GRNH/QFX=',2e11.4,' CANH/CANQFX=',2e11.4)
      CALL wrf_message( msg )
   END IF

   ! --- add heat and moisture fluxes to tendency variables

   CALL fire_tendency(grnhfx,grnqfx,canhfx,canqfx,    &  ! send
                      alfg,alfc,z1can,                &
                      zs,z_at_w,dz8w,mu,rho,          &
                      ids,ide, kds,kde, jds,jde,      &
                      ims,ime, kms,kme, jms,jme,      &
                      its,ite, kts,kte, jts,jte,      &
                      rthfrten,rqvfrten)                 ! recv

   RETURN

END SUBROUTINE cawfe

! =========================================================================

SUBROUTINE fire_topography( z,zs,                         &  ! incoming
                         ids,ide, kds,kde, jds,jde,            &
                         ims,ime, kms,kme, jms,jme,            &
                         its,ite, kts,kte, jts,jte,            &
                         ifms,ifme, kfms,kfme, jfms,jfme,      &
                         if_st,if_en,jf_st,jf_en,              &
                         nfrx,nfry,                            &
                         zf,zsf)                            ! outgoing
 
   USE module_wrf_error

   IMPLICIT NONE

! ----- incoming variables

   INTEGER, INTENT(in) :: ids,ide, kds,kde, jds,jde
   INTEGER, INTENT(in) :: ims,ime, kms,kme, jms,jme
   INTEGER, INTENT(in) :: its,ite, kts,kte, jts,jte
   INTEGER, INTENT(in) :: ifms,ifme, kfms,kfme, jfms,jfme
   INTEGER, INTENT(in) :: if_st,if_en,jf_st,jf_en 
   INTEGER, INTENT(in) :: nfrx,nfry

   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ):: zs
   REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: z

! ----- outgoing variables

   REAL, INTENT(out), DIMENSION( ifms:ifme,jfms:jfme ) :: zf,zsf

! ----- local variables

   INTEGER :: i,j,ii,jj
   INTEGER :: nvl
   INTEGER :: iu1

   REAL    :: t1
   REAL    :: fuelloadm,fuelload,fueldepth,qig,epsilon,rhob
   REAL    :: rtemp2,gammax,a,ratio,gamma,wn,rtemp1,etam,etas,ir,irm,xifr
   REAL    :: tii,tjj,epx,epy

   INTEGER  :: nfxy,nf,ni,nj,ci,cj,ip,jp

   CHARACTER (LEN=128) :: msg


! ----- begin subroutine

!
!     interpolates topography on atm grid to fire/fuel grid (zs -> zsf)
!     and also interpolates height above sea level of first grid 
!     point to fire grid (z -> zf).  Note that these are valid at
!     the center of the cell.

   DO j = jf_st,jf_en
       tjj = 1.5 + (FLOAT(j) - 1.5) / FLOAT(nfry)
       jj = INT(tjj)
       epy = tjj - FLOAT(jj)
       DO i = if_st,if_en
          tii = 1.5 + (FLOAT(i)-1.5)/FLOAT(nfrx)
          ii = INT(tii)
          epx = tii - FLOAT(ii)
          zsf(i,j) = (1.-epy)*((1.-epx)*zs(ii,jj)+epx*zs(ii+1,jj)) &
             + epy*((1.-epx)*zs(ii,jj+1)+epx*zs(ii+1,jj+1))
          zf(i,j) = (1.-epy)*((1.-epx)*z(ii,kds+1,jj)+epx*z(ii+1,kds+1,jj)) &
             + epy*((1.-epx)*z(ii,kds+1,jj+1)+epx*z(ii+1,kds+1,jj+1))
      enddo
   enddo

   END SUBROUTINE fire_topography


SUBROUTINE fire_startup( grid_id,                              & ! incoming
                         ifms,ifme, kfms,kfme, jfms,jfme,      &
                         if_st,if_en,jf_st,jf_en,              &
                         ifuelread,nfuel_cat0,                 &
                         zf,zsf,                               &
                         nfuel_cat,betafl,bbb,r_0,phiwc )  ! outgoing
 
!     ...  Initialize things that need to be set every time the
!          model starts up,
!          including restart, including constants, pre-multipliers,
!          and fuel map.

   USE module_fr_cawfe_fuel
   USE module_wrf_error

   IMPLICIT NONE

! ----- incoming variables

   INTEGER, INTENT(in) :: ifms,ifme, kfms,kfme, jfms,jfme
   INTEGER, INTENT(in) :: if_st,if_en,jf_st,jf_en 

   INTEGER, INTENT(in) :: ifuelread
   INTEGER, INTENT(in) :: nfuel_cat0

   INTEGER, INTENT(in) :: grid_id

   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme ) :: zf,zsf

! ----- outgoing variables

   INTEGER, INTENT(out), DIMENSION( ifms:ifme,jfms:jfme ) :: nfuel_cat

   REAL, INTENT(out), DIMENSION( ifms:ifme,jfms:jfme ) :: betafl,bbb,r_0,phiwc

! ----- local variables

   INTEGER :: i,j,ii,jj
   INTEGER :: nvl
   INTEGER :: iu1

   REAL    :: t1
   REAL    :: fuelloadm,fuelload,fueldepth,qig,epsilon,rhob
   REAL    :: rtemp2,gammax,a,ratio,gamma,wn,rtemp1,etam,etas,ir,irm,xifr
   REAL    :: tii,tjj,epx,epy

   INTEGER                                     :: nfxy,nf,ni,nj,ci,cj,ip,jp

   CHARACTER (LEN=80) :: lfile2
   CHARACTER (LEN=128) :: msg


! ----- begin subroutine

! ----- constants 

   xlm(1) = -.5
   xlm(3) = -.5
   xlm(2) = +.5
   xlm(4) = +.5
   ylm(1) = -.5
   ylm(2) = -.5
   ylm(3) = +.5
   ylm(4) = +.5


! ----- load fuel categories (or constant) here
!
!     ... How will fuel categories be set?  IFUELREAD= 0 - uniform; 
!            1 - user-prescribed algorithm; 2 - read files
 
   WRITE(msg,*)'fire_startup: ifuelread=', ifuelread
   CALL wrf_message ( msg )

   IF (ifuelread == 0) THEN
 
      DO j = jf_st,jf_en
         DO i = if_st,if_en
            nfuel_cat(i,j) = nfuel_cat0
         END DO
      END DO

   ELSE IF (ifuelread == 1) THEN
 
!         Make dependent on altitude (CO mountains/forest vs. plains)
!          2000 m : 6562 ft   ;    1600 m: 5249 ft

!        ... User defines fuel category spatial variability ! param! 

      DO j = jf_st,jf_en
         DO i = if_st,if_en
            nfuel_cat(i,j) = 2     ! Grass with understory
            t1 = zsf(i,j)
            IF (t1  <=  1524.) THEN   !  up to 5000 ft 
               nfuel_cat(i,j) = 3  ! Tall grass                   
            ELSE IF (t1 >= 1524. .AND. t1 <= 2073.) THEN  ! 5.0-6.8 kft.
               nfuel_cat(i,j) = 2  ! Grass with understory
            ELSE IF (t1 >= 2073. .AND. t1 <= 2438.) THEN  ! 6.8-8.0 kft.
               nfuel_cat(i,j) = 8  ! Timber litter - 10 (ponderosa)
            ELSE IF (t1 > 2438. .AND. t1 <=  3354.) THEN ! 8.0-11.0 kft.
!              ... could also be mixed conifer.
               nfuel_cat(i,j) = 10 ! Timber litter - 8 (lodgepole)
            ELSE IF (t1 > 3354. .AND. t1 <=  3658.) THEN ! 11.0-12.0 kft
               nfuel_cat(i,j) = 1  ! Alpine meadow - 1
            ELSE IF (t1 > 3658. ) THEN  ! > 12.0 kft
               nfuel_cat(i,j) = 14 ! No fuel.
            END IF
         END DO
      END DO

   ELSE IF (ifuelread == 2) THEN

! -- EGP need to fix this so that when reading fuel data from
!	a file that each if_st:if_en and jf_st:jf_en reads the 
!       correct chunk of the file....
!
!	NOTE: changed nvl=nvlm to nvl=model

! -----   written assuming NVERT=0, no parallelization
!        ...  Read fuel files 
         
!     nvl = nvlm      ! Load fuel for innermost domain.
      nvl = grid_id      ! Load fuel for innermost domain.

!jbeezley : file io here needs to conform to wrf standards :
!           should be put into registry if needed
      
      IF (nvl  <=  9) THEN
         WRITE(lfile2,80) nvl
80       FORMAT('fuel_layer_',I1,'.dat')
      ELSE
         WRITE (msg,*) 'STOP, fire_startup: Generalize filename format for NVL > 9'
         CALL wrf_error_fatal ( msg )
      END IF
 
      iu1 = 10
      OPEN(iu1,FILE=lfile2,STATUS='unknown',FORM='formatted')

      DO j = jf_st,jf_en
      DO i = if_st,if_en
         READ(iu1,'(I2)') nfuel_cat(i,j)
!        ... If no fuel category specified (i.e. '99'), set to '14', 
!            which (in the current 13 category NFFL category system) 
!            is 'no fuel'
         IF (nfuel_cat(i,j) < 1) nfuel_cat(i,j) = 14         ! not generalized
         IF (nfuel_cat(i,j) > nfuelcats) nfuel_cat(i,j) = 14 ! not generalized
      END DO
      END DO

      CLOSE (iu1)

   ELSE

       WRITE (msg,*) &
       'STOP, in fire_startup: error reading fuel categories from file: iu1' 
       CALL wrf_error_fatal ( msg )

   END IF

! ----- end  LOAD FUEL CATEGORIES (OR CONSTANT) HERE.

! -----  Settings of fire spread parameters from BEHAVE follows. These
!         don't need to be recalculated later.
!
   DO j = jf_st,jf_en
   DO i = if_st,if_en
      fuelloadm = (1.-bmst) * fgi(nfuel_cat(i,j))    ! fuelload without moisture
      fuelload = fuelloadm * (.3048)**2 * 2.205      ! to lb/ft^2
      fueldepth = fueldepthm(nfuel_cat(i,j))/0.3048  ! to ft
      betafl(i,j) = fuelload/(fueldepth * fueldens(nfuel_cat(i,j))) !packing ratio
      betaop = 3.348 * savr(nfuel_cat(i,j))**(-0.8189)     ! optimum packing ratio
      qig = 250. + 1116.*fuelmc_g                    ! heat of preigntn., btu/lb
      epsilon = EXP(-138./savr(nfuel_cat(i,j)) )     ! eff. heating number
      rhob = fuelload/fueldepth                  ! ovendry bulk density, lb/ft^3
  
      c = 7.47 * EXP( -0.133 * savr(nfuel_cat(i,j))**0.55) ! const in wind coef
      bbb(i,j) = 0.02526 * savr(nfuel_cat(i,j))**0.54      ! const in wind coef
      e = 0.715 * EXP( -3.59E-4 * savr(nfuel_cat(i,j)))    ! const in wind coef
      phiwc(i,j) = c * (betafl(i,j)/betaop)**(-e)         
 
      rtemp2 = savr(nfuel_cat(i,j))**1.5
      gammax = rtemp2/(495. + 0.0594*rtemp2)            ! maximum rxn vel, 1/min
      a = 1./(4.774 * savr(nfuel_cat(i,j))**0.1 - 7.27) ! coef for optimum rxn vel
      ratio = betafl(i,j)/betaop
      gamma = gammax *(ratio**a) *EXP(a*(1.-ratio))     ! optimum rxn vel, 1/min
 
      wn = fuelload/(1 + st(nfuel_cat(i,j)))       ! net fuel loading, lb/ft^2
      rtemp1 = fuelmc_g/fuelmce(nfuel_cat(i,j))
      etam = 1.-2.59*rtemp1 +5.11*rtemp1**2 -3.52*rtemp1**3 !moist damp coef
      etas = 0.174* se(nfuel_cat(i,j))**(-0.19)             !mineral damping coef
      ir = gamma * wn * fuelheat * etam * etas  ! rxn intensity, btu/ft^2 min
      irm = ir * 1055./( 0.3048**2 * 60.) * 1.e-6     !for MW/m^2

! ----- propagating flux ratio
      xifr = EXP( (0.792 + 0.681*savr(nfuel_cat(i,j))**0.5)  &
        * (betafl(i,j)+0.1)) /(192. + 0.2595*savr(nfuel_cat(i,j))) 
 
! -----  r_0 is the spread rate for a fire on flat ground with no wind.
      r_0(i,j) = ir*xifr/(rhob * epsilon *qig)    ! default spread rate in ft/min
      IF (nfuel_cat(i,j) == 14) r_0(i,j) = 0.   ! no fuel, no spread.
!     WRITE (msg,*) 'irm,r0 (m/s)=',i,j,irm, (r_0(i,j)/196.85)
!     CALL wrf_message ( msg )


   END DO
   END DO
 
   RETURN

END SUBROUTINE fire_startup

! =========================================================================

SUBROUTINE fire_init(iof,dt,                                       & ! incoming
                     cen_lat,cen_lon,lat_ll,lon_ll,                &
                     moad_cen_lat,moad_cen_lon,moad_dx,moad_dy,    &
                     moad_s_we,moad_e_we,moad_s_sn,moad_e_sn,      &
                     nfl_t,time,dx,dy,nfuel_cat,                   &
                     xcd,ycd,xcn,ycn,sprdx,sprdy,               &
                     ifds,ifde, kfds,kfde, jfds,jfde,              &
                     ifms,ifme, kfms,kfme, jfms,jfme,              &
                     if_st,if_en,jf_st,jf_en,                      &
                     nfrx,nfry,                                    &
                     ishape,tlat_stf,tlon_stf,t_ignite,icn,        &
                     fg,fc,tign_g,nfl,xfg,yfg,                     & ! in-out
                     ixb,iyb)                                        ! outgoing
 
! -------------------------------------------------------------------
!  If iof = 1:  this routine sets some variables to zero and initializes
!		some parameters
!
!  If iof = 2:  this routine ignites a fire with shape: ishape
! -------------------------------------------------------------------

   USE module_fr_cawfe_fuel

   IMPLICIT NONE

! ------ incoming variables

   INTEGER, INTENT(in) :: iof

!   INTEGER, INTENT(in) :: ids,ide, kds,kde, jds,jde
!   INTEGER, INTENT(in) :: ims,ime, kms,kme, jms,jme
!   INTEGER, INTENT(in) :: its,ite, kts,kte, jts,jte
   INTEGER, INTENT(in) :: ifds,ifde, kfds,kfde, jfds,jfde
   INTEGER, INTENT(in) :: ifms,ifme, kfms,kfme, jfms,jfme
   INTEGER, INTENT(in) :: if_st,if_en, jf_st,jf_en
   INTEGER, INTENT(in) :: nfrx,nfry

   REAL, INTENT(in)    :: cen_lat,cen_lon           ! lat,lon center of finest mesh
   REAL, INTENT(in)    :: lat_ll,lon_ll             ! lat,lon lower left of " "
   REAL, INTENT(in)    :: moad_cen_lat,moad_cen_lon ! lat,lon center of moad
   REAL, INTENT(in)    :: moad_dx,moad_dy           ! dx,dy of moad
   INTEGER, INTENT(in) :: moad_s_we,moad_e_we	    ! strt & end indices in x on moad
   INTEGER, INTENT(in) :: moad_s_sn,moad_e_sn       ! strt & end indices in y on moad

   INTEGER, INTENT(in) :: ishape
   REAL, INTENT(in)    :: tlat_stf
   REAL, INTENT(in)    :: tlon_stf
   REAL, INTENT(in)    :: t_ignite

   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme )  :: nfl_t,nfuel_cat
   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 ):: icn

   REAL, INTENT(in)  :: time,dt
   REAL, INTENT(in)  :: dx,dy

   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 )    :: xcd,ycd
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 )    :: xcn,ycn
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 )    :: sprdx,sprdy

! ------ outgoing variables

   INTEGER, INTENT(out), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: ixb,iyb

! ------ in and out going variables

   INTEGER, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: nfl

   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: fg,fc
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: tign_g
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xfg,yfg

! ------ local variables

   INTEGER :: i,j
   INTEGER :: it
   INTEGER :: istf,jstf
   INTEGER :: ii,jj
   INTEGER :: ixm,iym
   INTEGER :: i1,i2,j1,j2
   INTEGER :: nfl_max
   INTEGER :: ism,isp,jsm,jsp
   INTEGER :: nxmm,nymm
   INTEGER :: moad_num_pnts_x,moad_num_pnts_y

   REAL    :: xwidfire,ywidfire,tdiff,dxst,dyst,xcntr,ycntr,stx,sty
   REAL    :: xf1,yf1,xx1,yy1,fii,fjj
   REAL    :: rad,x0,y0,x1,y1,x2,y2
   REAL    :: rmin
   REAL    :: r1,r2,r3,r4
   REAL    :: r12,r13,r14,r23,r24,r34
   REAL    :: a_fl
   REAL    :: xwd,ywd
   REAL    :: xmax,ymax,rmax,alpha
   REAL    :: x,y,spd,ss,dxp,dyp

   CHARACTER(LEN=120) :: msg
 
! ------ begin routine

   IF (tignm  >  -10.) RETURN  ! If there is alrady a fire, return.
!           Generalize this when want to add multiple fires.
 
   IF (iof == 1) THEN   ! iof=1  
!        ... This is 1st time through sr. fire (either cold start or restart) and
!            NO FIRE YET. Initialize some fire & fuel variables that will change
!            once fire starts.
!
!        Define the initial tracer positions of the ground fuel (XFG,YFG)
!        where the coordinates are relative to the grid center such that
!        XFG and YFG vary between -0.5 to +0.5 where (XFG,YFG)=(0.,0.) is
!        the grid center. |XFG|.le. 0.5 and |YFG|.le. 0.5 .
!
      DO j = jf_st,jf_en
      DO i = if_st,if_en
         fg(i,j) = fgi(nfuel_cat(i,j)) !sfc fuel horiz. homogeneous conditions
         fc(i,j) = fci(nfuel_cat(i,j)) !canopy fuel horiz. homogeneous conditions
         nfl(i,j) = 0
         tign_g(i,j) = -100.           ! no fire set yet
         DO it = 1,4                   ! loop over the 4 tracers per fuel cell
            ixb(i,j,it) = 1
            iyb(i,j,it) = 1
            xfg(i,j,it) = 0.0
            yfg(i,j,it) = 0.0
         END DO
      END DO
      END DO
   END IF                       ! iof=1
 
   tdiff = ABS(t_ignite-time/60.)    ! T_IGNITE is in minutes

   IF (iof == 2 .AND. tdiff < dt/60.) THEN           ! iof=2
 
!       It's time to ignite a fire! (Generalize so that other fires could 
!          be burning elsewhere.)
!       T_IGNITE   is the designated ignition time in minutes (model time) 


! set tlat_stf,tlon_stf: latitude and longitude of the center of ignition location

      WRITE (msg,*) 'FIRE IGNITION AT: TLAT_STF, TLON_STF=',tlat_stf,tlon_stf
      CALL wrf_message ( msg )



! moad_cen_lat = latitude  of the center of outermost domain (mother of all domains)
! moad_cen_lon = longitude of the center of outermost domain
! get dxst,dyst: fire dist. from domain 1 center in m   

      dxst = (tlon_stf-moad_cen_lon) * 111.1949 * 1.e3  
      dyst = (tlat_stf-moad_cen_lat) * 111.1949 * 1.e3

! EGP
      dxst = 0.
      dyst = 0.

      WRITE (msg,*) 'fire dist dom1 center (m): dxst,dyst=',dxst,dyst
      CALL wrf_message ( msg )
      
! get xcntr,ycntr:  distance from SW corner domain 1 to center of domain 1 (m)

      moad_num_pnts_x = moad_e_we - moad_s_we + 1
      moad_num_pnts_y = moad_e_sn - moad_s_sn + 1

      xcntr = (FLOAT(moad_num_pnts_x) * moad_dx) / 2.
      ycntr = (FLOAT(moad_num_pnts_y) * moad_dy) / 2.

      WRITE (msg,*) 'dist SW corner dom1 to center dom1 (m): ', &
            'xcntr,ycntr=',xcntr,ycntr
      CALL wrf_message ( msg )


! get stx,sty: coords (in m) of fire start wrt the sw corner of model 1 in m

!     WRITE (msg,*) 'ycntr, dyst, sum=',ycntr,dyst, ycntr+dyst
!     CALL wrf_message ( msg )

      stx = xcntr + dxst  ! in m
      sty = ycntr + dyst

      WRITE (msg,*) &
      'coords fire rel to SW corner mod 1 (m): stx,sty:',stx,sty     
      CALL wrf_message ( msg )
            

! EGP --- FIX this for WRF... currently writing over
! get xf1,yf1: position of SW corner of fire domain (in m)
!
!     IF (ifs == 1) THEN
!        xf1 = (cen_lon - lon_ll) * 111.1949 * 1.e3
!        yf1 = (cen_lat - lat_ll) * 111.1949 * 1.e3
!     ELSE
!        xf1 = 999999.
!        yf1 = 999999.
!     END IF
!     xf1 = wrf_dm_min_real ( xf1 )
!     yf1 = wrf_dm_min_real ( yf1 )

      xf1 = 0.
      yf1 = 0.

      WRITE (msg,*) 'pos of sw corner of fire domain (m): xfx1,xfy1:',xf1,yf1
      CALL wrf_message ( msg )


! get xx1,yy1:  position of fire relative to fire domain (in m)
  
      xx1 = stx - xf1  ! in m
      yy1 = sty - yf1

      WRITE (msg,*) 'Fire position relative to fire domain (m):',xx1,yy1
      CALL wrf_message ( msg )


! get istf,jstf: position of fire in fire domain in atm grid cells

      istf = INT( xx1 / dx ) + 1  
      jstf = INT( yy1 / dy ) + 1  

      WRITE (msg,*) 'Fire position in domain atm grid pts:', istf,jstf
      CALL wrf_message ( msg )

! get fii,fjj: dist in m from this model level's SE corner

      fii= ( xx1 / dxf ) + 1. 
      fjj= ( yy1 / dyf ) + 1.

!     WRITE (msg,*) 'calc fii,fjj=',fii,fjj
!     CALL wrf_message ( msg )

      ii = AINT(fii)
      jj = AINT(fjj)

      WRITE (msg,*) 'Fire position in domain in fuel cells:',ii,jj
      CALL wrf_message ( msg )

!      .... Or set fire location this way.
!       ... ISTF LSTF  are the model dynamic grid positions of ignition point
!
!      istf =  31   !  param !  for 4 dom
!      jstf =  24   !  param !  for 4 dom
!      istf =  30   !  param !  for 3 dom
!      jstf =  38   !  param !  for 3 dom
!      istf = 104   !  param !  for 5 dom   ! could/should be 106
!      jstf =  49   !  param !  for 5 dom   ! could/should be 50
!      istf = 190   !  param !  for 6 dom
!      jstf =  68   !  param !  for 6 dom

       istf = moad_num_pnts_x / 2   !  param !
       jstf = moad_num_pnts_y / 2   !  param !
!
! --- EGP need to fix initialization for MPI...  currently the ishape
!	query happens on every CPU, therefore an equivalent fire will 
!	be initiated on every CPU
!
!  ------------------------------------------------------------------------
      IF (ishape == 0) THEN           ! spot fire 
!          ...The circular ignition that follows assumes physically small fuel
!             cells. Typical values of about 5 meters or less would be reasonable
!             otherwise use line ignition

         rad = 10.01 * SQRT(dxf**2 + dyf**2) ! rad= 1.01 * hypot of fuel cell ! param!

! set x0,y0: dist in m from this model level's SE corner

         x0 = FLOAT(ii-1) * dxf + .5*dxf 
         y0 = FLOAT(jj-1) * dyf + .5*dyf

         WRITE (msg,*) 'Spot fire initialized with rad=',rad,' at ', &
                ii,jj,' fuel cell location'
         CALL wrf_message ( msg )

         DO j = jf_st+1,jf_en-1
         DO i = if_st+1,if_en-1
!             ... calc distance of each corner of fuel cell from ig pt.
            r1 = SQRT( (FLOAT(i-2)*dxf-x0)**2 + (FLOAT(j-2)*dyf-y0)**2 )
            r2 = SQRT( (FLOAT(i-1)*dxf-x0)**2 + (FLOAT(j-2)*dyf-y0)**2 )
            r3 = SQRT( (FLOAT(i-2)*dxf-x0)**2 + (FLOAT(j-1)*dyf-y0)**2 )
            r4 = SQRT( (FLOAT(i-1)*dxf-x0)**2 + (FLOAT(j-1)*dyf-y0)**2 )
            rmin = MIN(r1,r2,r3,r4)

!             ... if any less than spot fire radius, ignite
            IF (rmin < rad) THEN         !rmin lt rad
               tign_g(i,j) = time
               nfl(i,j) = 1
               IF (rad-r1 > ep) THEN
                  xfg(i,j,1) = xlm(1)
                  yfg(i,j,1) = ylm(1)
               END IF
               IF (rad-r2 > ep) THEN
                 xfg(i,j,2) = xlm(2)
                 yfg(i,j,2) = ylm(2)
               END IF
               IF (rad-r3 > ep) THEN
                 xfg(i,j,3) = xlm(3)
                 yfg(i,j,3) = ylm(3)
               END IF
               IF (rad-r4 > ep) THEN
                 xfg(i,j,4) = xlm(4)
                 yfg(i,j,4) = ylm(4)
               END IF
 
               IF (r1 < rad .AND. r2 < rad .AND. r3 > rad .AND. r4 > rad) THEN
                  xfg(i,j,3) = xlm(3)
                  yfg(i,j,3) = ylm(1) + (rad-r1)/(r3-r1) 
                  xfg(i,j,4) = xlm(4)
                  yfg(i,j,4) = ylm(2) + (rad-r2)/(r4-r2) 
               END IF
               IF (r1 > rad .AND. r2 > rad .AND. r3 < rad .AND. r4 < rad) THEN
                  xfg(i,j,1) = xlm(1)
                  yfg(i,j,1) = ylm(3) - (rad-r3)/(r1-r3) 
                  xfg(i,j,2) = xlm(2)
                  yfg(i,j,2) = ylm(4) - (rad-r4)/(r2-r4) 
               END IF
               IF (r1 < rad .AND. r3 < rad .AND. r2 > rad .AND. r4 > rad) THEN
                  xfg(i,j,2) = xlm(1) + (rad-r1)/(r2-r1) 
                  yfg(i,j,2) = ylm(2)
                  xfg(i,j,4) = xlm(3) + (rad-r3)/(r4-r3) 
                  yfg(i,j,4) = ylm(4)
               END IF
               IF (r1 > rad .AND. r3 > rad .AND. r2 < rad .AND. r4 < rad) THEN
                  xfg(i,j,1) = xlm(2) - (rad-r2)/(r1-r2) 
                  yfg(i,j,1) = ylm(1)
                  xfg(i,j,3) = xlm(4) - (rad-r4)/(r3-r4) 
                  yfg(i,j,3) = ylm(3)
               END IF
  
               IF (r1 < rad .AND. r2 > rad .AND. r3 > rad .AND. r4 > rad) THEN
                  xfg(i,j,2) = xlm(1) + (rad-r1)/(r2-r1)
                  yfg(i,j,2) = ylm(2)
                  xfg(i,j,3) = xlm(3)
                  yfg(i,j,3) = ylm(1) + (rad-r1)/(r3-r1)
                  xfg(i,j,4) = .5*(xfg(i,j,2)+xfg(i,j,3))
                  yfg(i,j,4) = .5*(yfg(i,j,2)+yfg(i,j,3))
               END IF
               IF (r2 < rad .AND. r1 > rad .AND. r3 > rad .AND. r4 > rad) THEN
                  xfg(i,j,1) = xlm(2) - (rad-r2)/(r1-r2)
                  yfg(i,j,1) = ylm(1)
                  xfg(i,j,4) = xlm(4)
                  yfg(i,j,4) = ylm(2) + (rad-r2)/(r4-r2)
                  xfg(i,j,3) = .5*(xfg(i,j,1)+xfg(i,j,4))
                  yfg(i,j,3) = .5*(yfg(i,j,1)+yfg(i,j,4))
               END IF
               IF (r3 < rad .AND. r1 > rad .AND. r2 > rad .AND. r4 > rad) THEN
                  xfg(i,j,4) = xlm(3) + (rad-r3)/(r4-r3)
                  yfg(i,j,4) = ylm(4)
                  xfg(i,j,1) = xlm(1)
                  yfg(i,j,1) = ylm(3) - (rad-r3)/(r1-r3)
                  xfg(i,j,2) = .5*(xfg(i,j,1)+xfg(i,j,4))
                  yfg(i,j,2) = .5*(yfg(i,j,1)+yfg(i,j,4))
               END IF
               IF (r4 < rad .AND. r1 > rad .AND. r2 > rad .AND. r3 > rad) THEN
                  xfg(i,j,3) = xlm(4) - (rad-r4)/(r3-r4)
                  yfg(i,j,3) = ylm(3)
                  xfg(i,j,2) = xlm(2)
                  yfg(i,j,2) = ylm(4) - (rad-r4)/(r2-r4)
                  xfg(i,j,1) = .5*(xfg(i,j,2)+xfg(i,j,3))
                  yfg(i,j,1) = .5*(yfg(i,j,2)+yfg(i,j,3))
               END IF
  
               IF (r1 > rad .AND. r2 < rad .AND. r3 < rad .AND. r4 < rad) THEN
                  x1 = xlm(1)
                  x2 = xlm(2) - (rad-r2)/(r1-r2)
                  xfg(i,j,1) = .5*(x1+x2)
                  y1 = ylm(1)
                  y2 = ylm(3) - (rad-r3)/(r1-r3)
                  yfg(i,j,1) = .5*(y1+y2)
               END IF
               IF (r2 > rad .AND. r1 < rad .AND. r3 < rad .AND. r4 < rad) THEN
                    x1 = xlm(4)
                    x2 = xlm(1) + (rad-r1)/(r2-r1)
                    xfg(i,j,2) = .5*(x1+x2)
                    y1 = ylm(1)
                    y2 = ylm(4) - (rad-r4)/(r2-r4)
                    yfg(i,j,2) = .5*(y1+y2)
               END IF
               IF (r3 > rad .AND. r1 < rad .AND. r2 < rad .AND. r4 < rad) THEN
                    x1 = xlm(1)
                    x2 = xlm(4) - (rad-r4)/(r3-r4)
                    xfg(i,j,3) = .5*(x1+x2)
                    y1 = ylm(4)
                    y2 = ylm(1) + (rad-r1)/(r3-r1)
                    yfg(i,j,3) = .5*(y1+y2)
               END IF
               IF (r4 > rad .AND. r1 < rad .AND. r2 < rad .AND. r3 < rad) THEN
                    x1 = xlm(2)
                    x2 = xlm(3) + (rad-r3)/(r4-r3)
                    xfg(i,j,4) = .5*(x1+x2)
                    y1 = ylm(3)
                    y2 = ylm(2) + (rad-r2)/(r4-r2)
                    yfg(i,j,4) = .5*(y1+y2)
               END IF
 
               r12 = (xfg(i,j,1)-xfg(i,j,2))**2 + (yfg(i,j,1)-yfg(i,j,2))**2
               r13 = (xfg(i,j,1)-xfg(i,j,3))**2 + (yfg(i,j,1)-yfg(i,j,3))**2
               r14 = (xfg(i,j,1)-xfg(i,j,4))**2 + (yfg(i,j,1)-yfg(i,j,4))**2
               r23 = (xfg(i,j,2)-xfg(i,j,3))**2 + (yfg(i,j,2)-yfg(i,j,3))**2
               r24 = (xfg(i,j,2)-xfg(i,j,4))**2 + (yfg(i,j,2)-yfg(i,j,4))**2
               r34 = (xfg(i,j,3)-xfg(i,j,4))**2 + (yfg(i,j,3)-yfg(i,j,4))**2
               rmin = MIN(r12,r13,r14,r23,r24,r34)

               a_fl = .5*(                                                 &
                      (xfg(i,j,4)-xfg(i,j,1))*(yfg(i,j,3)-yfg(i,j,2))      &
                     +(yfg(i,j,4)-yfg(i,j,1))*(xfg(i,j,2)-xfg(i,j,3)))   

               IF (rmin < ep_sq .AND. a_fl < .5) THEN
                  IF (r12 < ep_sq) THEN
                    IF (ABS(xfg(i,j,3)-xlm(3)) < ep .AND.                 &
                               ABS(yfg(i,j,3)-ylm(3)) < ep) THEN
                      xfg(i,j,2) = .5*(xfg(i,j,1)+xfg(i,j,4))
                      yfg(i,j,2) = .5*(yfg(i,j,1)+yfg(i,j,4))
                    END IF
                    IF (ABS(xfg(i,j,4)-xlm(4)) < ep .AND.                 &
                               ABS(yfg(i,j,4)-ylm(4)) < ep) THEN
                      xfg(i,j,1) = .5*(xfg(i,j,2)+xfg(i,j,3))
                      yfg(i,j,1) = .5*(yfg(i,j,2)+yfg(i,j,3))
                    END IF
                  END IF
   
                  IF (r13 < ep_sq) THEN
                    IF (ABS(xfg(i,j,2)-xlm(2)) < ep .AND.                 &
                               ABS(yfg(i,j,2)-ylm(2)) < ep) THEN
                      xfg(i,j,3) = .5*(xfg(i,j,1)+xfg(i,j,4))
                      yfg(i,j,3) = .5*(yfg(i,j,1)+yfg(i,j,4))
                    END IF
                    IF (ABS(xfg(i,j,4)-xlm(4)) < ep .AND.                 &
                               ABS(yfg(i,j,4)-ylm(4)) < ep) THEN
                      xfg(i,j,1) = .5*(xfg(i,j,2)+xfg(i,j,3))
                      yfg(i,j,1) = .5*(yfg(i,j,2)+yfg(i,j,3))
                    END IF
                  END IF
   
                  IF (r24 < ep_sq) THEN
                    IF (ABS(xfg(i,j,3)-xlm(3)) < ep .AND.                 &
                                ABS(yfg(i,j,3)-ylm(3)) < ep) THEN
                      xfg(i,j,2) = .5*(xfg(i,j,1)+xfg(i,j,4))
                      yfg(i,j,2) = .5*(yfg(i,j,1)+yfg(i,j,4))
                    END IF
                    IF (ABS(xfg(i,j,1)-xlm(1)) < ep .AND.                 &
                                ABS(yfg(i,j,1)-ylm(1)) < ep) THEN
                      xfg(i,j,4) = .5*(xfg(i,j,2)+xfg(i,j,3))
                      yfg(i,j,4) = .5*(yfg(i,j,2)+yfg(i,j,3))
                    END IF
                  END IF
  
                  IF (r34 < ep_sq) THEN
                    IF (ABS(xfg(i,j,2)-xlm(2)) < ep .AND.                 &
                                ABS(yfg(i,j,2)-ylm(2)) < ep) THEN
                      xfg(i,j,3) = .5*(xfg(i,j,1)+xfg(i,j,4))
                      yfg(i,j,3) = .5*(yfg(i,j,1)+yfg(i,j,4))
                    END IF
                    IF (ABS(xfg(i,j,1)-xlm(1)) < ep .AND.                 &
                                ABS(yfg(i,j,1)-ylm(1)) < ep) THEN
                      xfg(i,j,4) = .5*(xfg(i,j,2)+xfg(i,j,3))
                      yfg(i,j,4) = .5*(yfg(i,j,2)+yfg(i,j,3))
                    END IF
                  END IF
               END IF
            END IF         !rmin lt rad
         END DO            ! i loop
         END DO            ! l loop
 
      END IF               ! spot fire
!
!  ------------------------------------------------------------------------
      if (ishape == 1) THEN          ! line fire
!
! ----- length and width settings....
!
!         ywidfire = 1. + 1.*dyf               ! N-S length in m   ! param!
!         ywidfire = 409.                      ! N-S length in m   ! param!
          ywidfire = 20.                       ! N-S length in m   ! param!
          xwidfire = 10.  ! 4                  ! W-E width in m    ! param!
!         xwidfire = dxf * .01                 ! narrow line, 1 cm ! param!

! ----- following is check on width - must be within 2*DXF as coded
          IF (xwidfire  >=  2.*dxf) xwidfire = 2.*dxf

! ----- width of fire line in non-dim lenth
          xwd = xwidfire/dxf * .5 

! ----- N-S length fire line in fuel cells
          iym = INT(ywidfire/dyf)

! ----- length of end pieces
          ywd = (ywidfire - FLOAT(iym)*dyf)/dyf/2.
          ywd = MAX(ywd,2.*ep)
 
! ----- Location.....
          j1 = jj - (iym/2)
          j2 = j1 + iym
          i1 = ii
          i2 = i1 + 1

! ----- fire length in m = (j2-j1)*dyf or (i2-i1)*dxf
 
          IF (iym > 0) THEN    !fire line longer than one fuel cell 
             DO j = j1,j2
                tign_g(i1,j) = time
                tign_g(i2,j) = time
                nfl(i1,j) = 1
                nfl(i2,j) = 1
                xfg(i1,j,1) = +.5 - xwd  ! left half of fire line
                xfg(i1,j,2) = +.5
                xfg(i1,j,3) = +.5 - xwd
                xfg(i1,j,4) = +.5
                yfg(i1,j,1) = -.5
                yfg(i1,j,2) = -.5
                yfg(i1,j,3) = +.5
                yfg(i1,j,4) = +.5
                xfg(i2,j,1) = -.5        ! right half of fire line
                xfg(i2,j,2) = -.5 + xwd
                xfg(i2,j,3) = -.5
                xfg(i2,j,4) = -.5 + xwd
                yfg(i2,j,1) = -.5
                yfg(i2,j,2) = -.5
                yfg(i2,j,3) = +.5
                yfg(i2,j,4) = +.5
             END DO
             nfl_max = nfl_max + 1
          END IF                          !fire line longer than one fuel cell 
 
          IF (iym == 0)  j2 = j1 - 1

          tign_g(i1,j2+1) = time
          tign_g(i2,j2+1) = time
          tign_g(i1,j1-1) = time
          tign_g(i2,j1-1) = time
          nfl(i1,j2+1) = 1
          nfl(i2,j2+1) = 1
          nfl(i1,j1-1) = 1
          nfl(i2,j1-1) = 1
          nfl_max = nfl_max + 4
 
          xfg(i1,j2+1,1) = +.5 - xwd
          xfg(i1,j2+1,2) = +.5
          xfg(i1,j2+1,3) = +.5 - xwd*.5
          xfg(i1,j2+1,4) = +.5
          yfg(i1,j2+1,1) = -.5
          yfg(i1,j2+1,2) = -.5
          yfg(i1,j2+1,3) = -.5 + ywd*.5
          yfg(i1,j2+1,4) = -.5 + ywd
 
          xfg(i2,j2+1,1) = -.5
          xfg(i2,j2+1,2) = -.5 + xwd
          xfg(i2,j2+1,3) = -.5
          xfg(i2,j2+1,4) = -.5 + xwd*.5
          yfg(i2,j2+1,1) = -.5
          yfg(i2,j2+1,2) = -.5
          yfg(i2,j2+1,3) = -.5 + ywd
          yfg(i2,j2+1,4) = -.5 + ywd*.5
 
          xfg(i1,j1-1,1) = +.5 - xwd*.5
          xfg(i1,j1-1,2) = +.5
          xfg(i1,j1-1,3) = +.5 - xwd
          xfg(i1,j1-1,4) = +.5
          yfg(i1,j1-1,1) = +.5 - ywd*.5
          yfg(i1,j1-1,2) = +.5 - ywd
          yfg(i1,j1-1,3) = +.5
          yfg(i1,j1-1,4) = +.5
 
          xfg(i2,j1-1,1) = -.5
          xfg(i2,j1-1,2) = -.5 + xwd*.5
          xfg(i2,j1-1,3) = -.5
          xfg(i2,j1-1,4) = -.5 + xwd
          yfg(i2,j1-1,1) = +.5 - ywd
          yfg(i2,j1-1,2) = +.5 - ywd*.5
          yfg(i2,j1-1,3) = +.5
          yfg(i2,j1-1,4) = +.5

      END IF                         !line fire

!  ------------------------------------------------------------------------

      IF (ishape == 2) THEN          !windmill fire


!jm WARNING: DEAD CODE
!jm was in atm grid indices (error?), change to fire grid indices
!         nxmm = (ide-ids+1)-2
!         nymm = (jde-jds+1)-2

         nxmm = (ifde-ifds+1)-2
         nymm = (jfde-jfds+1)-2

         iym = 14
         j1 = 2 + nfry*(nymm/2-iym-1)
         j2 = 1 + nfry*(1+nymm/2+iym)
         ixm = 14
         i1 = 2 + nfrx*(nxmm/2-ixm-1)
         i2 = 1 + nfrx*(1+nxmm/2+ixm)

! ----- Straight line fire
!        iym = 10
!        j1 = 2 + nfry*(nymm/2-iym-1)
!        j2 = 1 + nfry*(1+nymm/2+iym)
!        ixm = 1
!        i1 = 2 + nfrx*(nxmm/2-ixm-1)
!        i2 = 1 + nfrx*(1+nxmm/2+ixm)
         i = 1 + nfrx*nxmm/2
!
         xmax = FLOAT(nxmm/2)*dx
         ymax = FLOAT(nymm/2)*dy
         rmax = SQRT(xmax**2+ymax**2)
         alpha = +4.*FLOAT(nxmm)/60.

         jsm = (nymm/2)*nfry
         jsp = (nymm/2)*nfry+3
         jsm = jsm - 2
         jsp = jsp + 2
!
         DO j = j1,j2
!           IF (j == (nymm/2)*nfry+1 .OR. j == (nymm/2)*nfry+2) GO TO 801
            IF (j >= jsm+1 .AND. j <= jsp-1) GO TO 801
            tign_g(i,j) = time
            tign_g(i+1,j) = time
            xfg(i,j,1) = +.25
            xfg(i,j,2) = +.50
            xfg(i,j,3) = +.25
            xfg(i,j,4) = +.50
            yfg(i,j,1) = -.5
            yfg(i,j,2) = -.5
            yfg(i,j,3) = +.5
            yfg(i,j,4) = +.5
            xfg(i+1,j,1) = -.50
            xfg(i+1,j,2) = -.25
            xfg(i+1,j,3) = -.50
            xfg(i+1,j,4) = -.25
            yfg(i+1,j,1) = -.5
            yfg(i+1,j,2) = -.5
            yfg(i+1,j,3) = +.5
            yfg(i+1,j,4) = +.5
         END DO
!
 801     CONTINUE
 
         yfg(i+1,jsp,1) = -.25
         yfg(i+1,jsp,2) = -.25
         yfg(i  ,jsp,1) = -.25
         yfg(i  ,jsp,2) = -.25
         yfg(i+1,jsm,3) = +.25
         yfg(i+1,jsm,4) = +.25
         yfg(i  ,jsm,3) = +.25
         yfg(i  ,jsm,4) = +.25
!
         y = (j2-((jfde-jfds+1)-2)/2-1.5)*dyf   !grid center
         spd = alpha*ABS(y)/rmax
         ss = 0.018*EXP(.8424*spd)    ! why Macarthur in here?
         dyp = (0.018*dxf)/(ss*dyf)
         dyp = SIGN(1.,alpha)*dyp

         yfg(i+1,j2,3) = +.25
         yfg(i+1,j2,4) = +.25 - .5*dyp
         yfg(i  ,j2,3) = +.25 + .5*dyp
         yfg(i  ,j2,4) = +.25
!
         yfg(i+1,j1,1) = -.25
         yfg(i+1,j1,2) = -.25 - .5*dyp
         yfg(i  ,j1,1) = -.25 + .5*dyp
         yfg(i  ,j1,2) = -.25

! ----- FIRE WIDTH = .01*dyf
         j = 1 + nfry*nymm/2
         ism = (nxmm/2)*nfrx
         isp = (nxmm/2)*nfrx+3
         ism = ism - 2
         isp = isp + 2
 
         DO i = i1,i2
!           IF (i == (nxmm/2)*nfrx+1 .OR. i == (nxmm/2)*nfrx+2) GO TO 799
            IF (i >= ism+1 .AND. i <= isp-1) GO TO 799
            tign_g(i,j) = time
            tign_g(i,j+1) = time
            xfg(i,j,1) = -.5
            xfg(i,j,2) = +.5
            xfg(i,j,3) = -.5
            xfg(i,j,4) = +.5
            yfg(i,j,1) = +.25
            yfg(i,j,2) = +.25
            yfg(i,j,3) = +.50
            yfg(i,j,4) = +.50
 
            xfg(i,j+1,1) = -.5
            xfg(i,j+1,2) = +.5
            xfg(i,j+1,3) = -.5
            xfg(i,j+1,4) = +.5
            yfg(i,j+1,1) = -.50
            yfg(i,j+1,2) = -.50
            yfg(i,j+1,3) = -.25
            yfg(i,j+1,4) = -.25
         END DO
 
  799    CONTINUE
 
! ---- EGP need to make sure only the grid that contains
!	the fire center gets set
!
         x = (i2-(nfrx*(ifde-ifds+1)-2)/2-1.5)*dxf   !grid center
         spd = alpha*ABS(x)/rmax
         ss = 0.018*EXP(.8424*spd)    ! why Macarthur in here?
         dxp = (0.018*dyf)/(ss*dxf)
         dxp = SIGN(1.,alpha)*dxp
 
         xfg(i1,j+1,3) = -.25 + .5*dxp
         xfg(i1,j+1,1) = -.25
         xfg(i1,j  ,3) = -.25
         xfg(i1,j  ,1) = -.25 - .5*dxp
 
!        ism = (nxmm/2)*nfrx
!        isp = (nxmm/2)*nfrx + 3
         xfg(ism,j+1,4) = +.25
         xfg(ism,j+1,2) = +.25
         xfg(ism,j  ,4) = +.25
         xfg(ism,j  ,2) = +.25
         xfg(isp,j+1,3) = -.25
         xfg(isp,j+1,1) = -.25
         xfg(isp,j  ,3) = -.25
         xfg(isp,j  ,1) = -.25
 
         xfg(i2,j+1,4) = +.25 + .5*dxp
         xfg(i2,j+1,2) = +.25
         xfg(i2,j  ,4) = +.25
         xfg(i2,j  ,2) = +.25 - .5*dxp
      END IF                         ! END ishape=2, windmill fire

      tignm = time

   END IF                       ! iof=2

   RETURN

END SUBROUTINE fire_init

! =========================================================================

SUBROUTINE fire_stat(iffg,dt,time,                    &  ! incoming
                      xcd,ycd,xcn,ycn,sprdx,sprdy,  &
                     ifms,ifme, kfms,kfme, jfms,jfme, &
                     if_st,if_en,jf_st,jf_en,         &
                     nfl,nfl_t,tign_g,tign_crt,       &  ! inout
                     area,area2,xfg,yfg,              &
                     ixb,iyb,icn)                        ! out

! --- if iffg > 0, this routine resets all the variables
!     defining the fire line, updates the locations of
!     the fire line, and ignites any cells fully surrounded
!     fire

   IMPLICIT NONE

! ----- incoming variables

!   INTEGER, INTENT(in) :: ids,ide, kds,kde, jds,jde
!  INTEGER, INTENT(in) :: ims,ime, kms,kme, jms,jme
!   INTEGER, INTENT(in) :: its,ite, kts,kte, jts,jte
   INTEGER, INTENT(in) :: ifms,ifme, kfms,kfme, jfms,jfme
   INTEGER, INTENT(in) :: if_st,if_en, jf_st,jf_en

   INTEGER, INTENT(in) :: iffg

   REAL, INTENT(in)    :: dt,time

   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 )    :: xcd,ycd
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 )    :: xcn,ycn
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 )    :: sprdx,sprdy

! ----- inout variables

   INTEGER, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme ) :: nfl,nfl_t

   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )    :: tign_g,tign_crt
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )    :: area,area2
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 )  :: xfg,yfg

! ----- outgoing variables

   INTEGER, INTENT(out), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: ixb,iyb,icn

! ----- local variables

   INTEGER :: i,j,it
   INTEGER :: i1,j1
   INTEGER :: nct,icls
   INTEGER :: ic1,ic2,ic3,ic4
   INTEGER :: inxt,inyt
   INTEGER :: is,js,iss,jss
   INTEGER :: iod
   INTEGER :: isum,jsum
   INTEGER :: itest
   INTEGER :: itot
   INTEGER :: ita,itb,itc

   INTEGER, DIMENSION( ifms:ifme,jfms:jfme ) :: nc,icl,islsum
   CHARACTER(LEN=256) :: msg

   REAL :: x1,y1
   REAL :: x2,y2
   REAL :: x3,y3
   REAL :: x4,y4
   REAL :: an
   REAL :: t1
   REAL :: xavg,yavg
   REAL :: xfg_a,yfg_a

   REAL, DIMENSION( ifms:ifme,jfms:jfme ) :: tmp
 
! ----- when deriving fireline coordinates we always keep the fire to our left
 
   DO j = jf_st,jf_en
   DO i = if_st,if_en
      DO it = 1,4
         ixb(i,j,it) = INT(ABS(xfg(i,j,it)-xlm(it)) /  &
                          (ABS(xfg(i,j,it)-xlm(it)) + ep) + .5 - ep)
         iyb(i,j,it) = INT(ABS(yfg(i,j,it)-ylm(it)) /  &
                          (ABS(yfg(i,j,it)-ylm(it)) + ep) + .5 - ep)
         icn(i,j,it) = 1 + ixb(i,j,it)*iyb(i,j,it) - ixb(i,j,it) - iyb(i,j,it)
      END DO
   END DO
   END DO
 
   IF (iffg > 0) THEN    ! iffg > 0

      DO j = jf_st,jf_en
      DO i = if_st,if_en
         nc(i,j)  = icn(i,j,1) + icn(i,j,2) + icn(i,j,3) + icn(i,j,4)
         icl(i,j) = ixb(i,j,1) + ixb(i,j,2) + ixb(i,j,3) + ixb(i,j,4)         &
                  + iyb(i,j,1) + iyb(i,j,2) + iyb(i,j,3) + iyb(i,j,4)
         tmp(i,j) = .5 * ((xfg(i,j,4)-xfg(i,j,1)) * (yfg(i,j,3)-yfg(i,j,2))   &
                        + (yfg(i,j,4)-yfg(i,j,1)) * (xfg(i,j,2)-xfg(i,j,3)))
      END DO
      END DO
 
      DO j = jf_st+1,jf_en-1
      DO i = if_st+1,if_en-1

         IF (nfl(i,j) == 1) THEN     !NFL=1  LOOP

            nct = nc(i,j)
            icls = icl(i,j)

            IF (nct == 3 .AND. icls == 1) THEN  !3/1 TREATMENT

! ----- natural triangle is (x1,x2) (x2,y2), (x3,y3)

               ic1 = 10 - icn(i,j,1) - 2*icn(i,j,2) - 3*icn(i,j,3) - 4*icn(i,j,4)
               x1 = xlm(ic1)
               y1 = ylm(ic1)
               x2 = xfg(i,j,ic1)
               y2 = yfg(i,j,ic1)
 
               ic3 = (1 - icn(i,j,1))*(2 + ixb(i,j,1))    &
                   + (1 - icn(i,j,2))*(1 + 3*ixb(i,j,2))  &
                   + (1 - icn(i,j,3))*(4 - 3*ixb(i,j,3))  &
                   + (1 - icn(i,j,4))*(3 - ixb(i,j,4)) 
               x3 = xlm(ic3)
               y3 = ylm(ic3)

               inxt = (1 - ixb(i,j,ic1))*(          & ! 0=no virt, 1=virt x-coord
                  (1-icn(i,j,1)) * (1-iyb(i,j-1,3)) +  &
                  (1-icn(i,j,2)) * (1-iyb(i,j-1,4)) +  &
                  (1-icn(i,j,3)) * (1-iyb(i,j+1,1)) +  &
                  (1-icn(i,j,4)) * (1-iyb(i,j+1,2)) )
 
               x3 = FLOAT(1-inxt)*x3 + FLOAT(inxt)*(   &
                  FLOAT(1-icn(i,j,1)) * xfg(i,j-1,3)+  &
                  FLOAT(1-icn(i,j,2)) * xfg(i,j-1,4)+  &
                  FLOAT(1-icn(i,j,3)) * xfg(i,j+1,1)+  &
                  FLOAT(1-icn(i,j,4)) * xfg(i,j+1,2))
 
               inyt = ixb(i,j,ic1)*(                & ! 0=no virt, 1=virt y-coord
                  (1-icn(i,j,1)) * (1-ixb(i-1,j,2)) +  &
                  (1-icn(i,j,2)) * (1-ixb(i+1,j,1)) +  &
                  (1-icn(i,j,3)) * (1-ixb(i-1,j,4)) +  &
                  (1-icn(i,j,4)) * (1-ixb(i+1,j,3)) )

               y3 = FLOAT(1-inyt)*y3 + FLOAT(inyt) * ( &
                  FLOAT(1-icn(i,j,1))*yfg(i-1,j,2) +   &
                  FLOAT(1-icn(i,j,2))*yfg(i+1,j,1) +   &
                  FLOAT(1-icn(i,j,3))*yfg(i-1,j,4) +   &
                  FLOAT(1-icn(i,j,4))*yfg(i+1,j,3) )
 
               tmp(i,j) = 1. - .5*ABS((x2-x1)*(y3-y1)) - .5*ABS((y2-y1)*(x3-x1))

               IF (tmp(i,j)  >  1.-ep) THEN
                  IF (iffg == 1) tmp(i,j) = 1. - 2.*ep   !test
  
                  xfg(i,j-1,3) = FLOAT(icn(i,j,1))*xfg(i,j-1,3)+  & 
                                 FLOAT(1-icn(i,j,1))*xlm(3)   
                  xfg(i,j-1,4) = FLOAT(icn(i,j,2))*xfg(i,j-1,4)+  &
                                 FLOAT(1-icn(i,j,2))*xlm(4)   
                  xfg(i,j+1,1) = FLOAT(icn(i,j,3))*xfg(i,j+1,1)+  & 
                                 FLOAT(1-icn(i,j,3))*xlm(1)   
                  xfg(i,j+1,2) = FLOAT(icn(i,j,4))*xfg(i,j+1,2)+  & 
                                 FLOAT(1-icn(i,j,4))*xlm(2)   
    
                  yfg(i-1,j,2) = FLOAT(icn(i,j,1))*yfg(i-1,j,2)+  & 
                                 FLOAT(1-icn(i,j,1))*ylm(2)   
                  yfg(i+1,j,1) = FLOAT(icn(i,j,2))*yfg(i+1,j,1)+  & 
                                 FLOAT(1-icn(i,j,2))*ylm(1)   
                  yfg(i-1,j,4) = FLOAT(icn(i,j,3))*yfg(i-1,j,4)+  & 
                                 FLOAT(1-icn(i,j,3))*ylm(4)   
                  yfg(i+1,j,3) = FLOAT(icn(i,j,4))*yfg(i+1,j,3)+  & 
                                 FLOAT(1-icn(i,j,4))*ylm(3)   

               END IF
            END IF                            !3/1 TREATMENT
 
            IF (nct == 2 .AND. icls == 3) THEN    !2/3 TREATMENT

! ----- We convert 2/3's to 2/2's because code has no treatment
!       for 2/3's, i.e. it is an unecessary class.
!      IC1 and IC2 are indices of the two corners
!      IC3 is index of point to be moved to near IC1 corner.
 
               ic1 = 1*icn(i,j,1)*(ixb(i,j,3)+iyb(i,j,2)) + &
                     2*icn(i,j,2)*(ixb(i,j,4)+iyb(i,j,1)) + &
                     3*icn(i,j,3)*(ixb(i,j,1)+iyb(i,j,4)) + &
                     4*icn(i,j,4)*(ixb(i,j,2)+iyb(i,j,3))

               ic2 = 1*icn(i,j,1)*ixb(i,j,4)*iyb(i,j,4) + &
                     2*icn(i,j,2)*ixb(i,j,3)*iyb(i,j,3) + &
                     3*icn(i,j,3)*ixb(i,j,2)*iyb(i,j,2) + &
                     4*icn(i,j,4)*ixb(i,j,1)*iyb(i,j,1)
 
               ic3 =  ixb(i,j,1)*iyb(i,j,1) + &
                    2*ixb(i,j,2)*iyb(i,j,2) + &
                    3*ixb(i,j,3)*iyb(i,j,3) + &
                    4*ixb(i,j,4)*iyb(i,j,4)

               ic4 = 10 - ic1 - ic2 - ic3

               is = i + (1-ixb(i,j,ic4))*INT((1.+ep)*(xfg(i,j,ic1)-xfg(i,j,ic4)))
               js = j +    ixb(i,j,ic4) *INT((1.+ep)*(yfg(i,j,ic1)-yfg(i,j,ic4)))
 
               xfg(i,j,ic3) = FLOAT(iyb(i,j,ic4))*xlm(ic3) &
                   + FLOAT(1-iyb(i,j,ic4))* &
                      (FLOAT(1-nfl(i,js)*ixb(i,js,ic4))* &
                      (xlm(ic1)+SIGN(2.*ep,xfg(i,j,ic3)-xfg(i,j,ic1)))     &
!                  + nfl(i,js)*ixb(i,js,ic4)*xfg(i,js,ic4))        & !old version
                 + nfl(i,js)*ixb(i,js,ic4)*((1-iyb(i,js,ic4))*xfg(i,js,ic4) &!test
                 + iyb(i,js,ic4)*(-xlm(ic3)+SIGN(2.*ep,xlm(ic3)))))

!                ixb(i,j,ic3) = 1 - iyb(i,j,ic4)                    !old version
               ixb(i,j,ic3) = INT(ABS(xfg(i,j,ic3)-xlm(ic3))/     &
                                 (ABS(xfg(i,j,ic3)-xlm(ic3))+ep)+.5-ep)  !test 
 
               yfg(i,j,ic3) = FLOAT(ixb(i,j,ic4))*ylm(ic3)                  &
                 + FLOAT(1-ixb(i,j,ic4))*(FLOAT(1-nfl(is,j)*iyb(is,j,ic4))* &
                      (ylm(ic1)+SIGN(2.*ep,yfg(i,j,ic3)-yfg(i,j,ic1)))      &
!                  + nfl(is,j)*iyb(is,j,ic4)*yfg(is,j,ic4))         &  !old version
                 + nfl(is,j)*iyb(is,j,ic4)*((1-ixb(is,j,ic4))*yfg(is,j,ic4) & !test
                 + ixb(is,j,ic4)*(-ylm(ic3)+SIGN(2.*ep,ylm(ic3)))))

!                iyb(i,j,ic3) = 1 - ixb(i,j,ic4)                     !old version
               iyb(i,j,ic3) = INT(ABS(yfg(i,j,ic3)-ylm(ic3))/      &
                                 (ABS(yfg(i,j,ic3)-ylm(ic3))+ep)+.5-ep)  !test

               tmp(i,j) = .5*((xfg(i,j,4)-xfg(i,j,1))*(yfg(i,j,3)-yfg(i,j,2)) &
                             +(yfg(i,j,4)-yfg(i,j,1))*(xfg(i,j,2)-xfg(i,j,3)))
 
               IF (nfl(is,js) == 0) THEN
                  xfg(is,js,ic2) = xlm(ic2)
                  yfg(is,js,ic2) = ylm(ic2)
                  xfg(is,js,ic4) = FLOAT(1-iyb(i,j,ic3))*xfg(i,j,ic3) &
                                 + FLOAT(  iyb(i,j,ic3))*xlm(ic4)
                  yfg(is,js,ic4) = FLOAT(  iyb(i,j,ic3))*yfg(i,j,ic3) &
                                 + FLOAT(1-iyb(i,j,ic3))*ylm(ic4)
 
                  iss = is + INT(SIGN(1.,xfg(i,j,ic2)-xfg(i,j,ic4)))  &
                               * (1-iyb(i,j,ic3))
                  jss = js + INT(SIGN(1.,yfg(i,j,ic2)-yfg(i,j,ic4)))  &
                               *    iyb(i,j,ic3)

!                   WRITE(msg,*) 'debug STAT 23 CONVERSION I J=',i,j 
!                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!                   WRITE(msg,*) 'debug IS JS ISS JSS=',is,js,iss,jss 
!                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)

                  xfg(is,js,ic1) = FLOAT(1-iyb(i,j,ic3))*xlm(ic1)    &
                    + FLOAT(iyb(i,j,ic3))*(                          &
                        FLOAT(nfl(iss,jss)*(1-iyb(iss,jss,ic3)))     &
                       * xfg(iss,jss,ic3)                            &
                       + FLOAT(1-nfl(iss,jss)*(1-iyb(iss,jss,ic3)))* &
                           (xlm(ic2)+2.*ep*SIGN(1.,xfg(i,j,ic1)-xfg(i,j,ic2))))

                  yfg(is,js,ic1) = FLOAT(iyb(i,j,ic3))*ylm(ic1)      &
                    + FLOAT(1-iyb(i,j,ic3))*(                        &
                       FLOAT(nfl(iss,jss)*(1-ixb(iss,jss,ic3)))      &
                      * yfg(iss,jss,ic3)                             &
                      + FLOAT(1-nfl(iss,jss)*(1-ixb(iss,jss,ic3)))*  &
                           (ylm(ic2)+2.*ep*SIGN(1.,yfg(i,j,ic1)-yfg(i,j,ic2))))

                  xfg(is,js,ic3) = .5*(xfg(is,js,ic1)+xfg(is,js,ic4))
                  yfg(is,js,ic3) = .5*(yfg(is,js,ic1)+yfg(is,js,ic4))

                  nfl(is,js) = 1
                  tign_g(is,js) = time

!                 WRITE(msg,*) 'debug XFG(IS,JS=',(XFG(IS,JS,IT),IT=1,4) 
!                 call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!                 WRITE(msg,*) 'debug YFG(IS,JS=',(YFG(IS,JS,IT),IT=1,4) 
!                 call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!                 WRITE(msg,*) 'debug IC1 IC2 IC3 IC4=',IC1,IC2,IC3,IC4 
!                 call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!
               END IF
            END IF                              !2/3 TREATMENT
 
            IF (nct == 3 .AND. icls == 2) THEN    !3/2 TREATMENT

               ic1 = 1*(ixb(i,j,1)*iyb(i,j,1)) + &
                     2*(ixb(i,j,2)*iyb(i,j,2)) + &
                     3*(ixb(i,j,3)*iyb(i,j,3)) + &
                     4*(ixb(i,j,4)*iyb(i,j,4))
               ic2 = 10 - icn(i,j,1) - 2*icn(i,j,2) - 3*icn(i,j,3) - 4*icn(i,j,4)

               x1 = xfg(i,j,ic1)
               y1 = yfg(i,j,ic1)
               x2 = xlm(ic2)
               y2 = ylm(ic2)
 
               iod = ic1 - 2*(ic1/2)
               ic3 = ic1 - 1 + 2*iod
               ic4 = 6 - ic1 - 2*iod
               is = i + 1 - 2*iod
               js = j - 1 + 2*(ic1/3)
 
               x3 = FLOAT(1-nfl(i,js)*ixb(i,js,ic4))*xlm(ic3) +        &
                    FLOAT(  nfl(i,js)*ixb(i,js,ic4))*xfg(i,js,ic4)
               y3 = ylm(ic3)
               x4 = xlm(ic4)
               y4 = FLOAT(1-nfl(is,j)*iyb(is,j,ic3))*ylm(ic4) +        &
                    FLOAT(  nfl(is,j)*iyb(is,j,ic3))*yfg(is,j,ic3)

               tmp(i,j) = 1. - .5*(ABS((x2-x1)*(y3-y4)) + ABS((y2-y1)*(x4-x3)))
 
            END IF                              !3/2 TREATMENT
 
         END IF                      !NFL=1  LOOP

      END DO
      END DO
 
      IF (iffg == 1) THEN    ! iffg == 1

         DO j = jf_st,jf_en
         DO i = if_st,if_en
            area(i,j) = tmp(i,j)
         END DO
         END DO

      END IF                 ! iffg == 1
 
      IF(IFFG == 2) THEN    !IFFG == 2
 
! --- locate and ignite any totally boundary enclosed regions, i.e. where
!     nfl=1 everywhere within a closed burning contour. We only consider
!     regions with area less than 1.-ep

         DO j = jf_st,jf_en
         DO i = if_st,if_en
            an = .5*((xfg(i,j,4)-xfg(i,j,1))*(yfg(i,j,3)-yfg(i,j,2))  &
                   + (yfg(i,j,4)-yfg(i,j,1))*(xfg(i,j,2)-xfg(i,j,3)))
            nfl_t(i,j) = INT((ep+tign_g(i,j)+ABS(ep+tign_g(i,j)))/    &
                  (2.*ABS(tign_g(i,j))+2.*ep)+.5)    
            nfl(i,j) = nfl_t(i,j) * (1 - INT(an+ep))
         END DO
         END DO

         DO j = jf_st,jf_en
         DO i = if_st,if_en
            DO it = 1,4
               ixb(i,j,it) = INT(ABS(xfg(i,j,it)-xlm(it))/  &
                             (ABS(xfg(i,j,it)-xlm(it)) + ep) + .5 - ep)
               iyb(i,j,it) = INT(ABS(yfg(i,j,it)-ylm(it))/  &
                             (ABS(yfg(i,j,it)-ylm(it)) + ep) + .5 - ep)
               icn(i,j,it) = 1 + ixb(i,j,it)*iyb(i,j,it)    &
                               - ixb(i,j,it) - iyb(i,j,it)
            END DO
         END DO
         END DO

         DO j = jf_st,jf_en
         DO i = if_st,if_en
            nc(i,j)  = icn(i,j,1) + icn(i,j,2) + icn(i,j,3) + icn(i,j,4)
            icl(i,j) = ixb(i,j,1) + ixb(i,j,2) + ixb(i,j,3) + ixb(i,j,4) +  &
                       iyb(i,j,1) + iyb(i,j,2) + iyb(i,j,3) + iyb(i,j,4)         
         END DO
         END DO
 
         DO j = jf_st+1,jf_en-1
         DO i = if_st+1,if_en-1

            IF (nfl(i,j) == 1) THEN     !NFL=1
               islsum(i,j) =                                              &
                  INT((tign_g(i+1,j) + ABS(tign_g(i+1,j)) +  2.*ep)/      &
                                   (2.*ABS(tign_g(i+1,j)) + 1.5*ep)) +    &
                  INT((tign_g(i-1,j) + ABS(tign_g(i-1,j)) +  2.*ep)/      &
                                   (2.*ABS(tign_g(i-1,j)) + 1.5*ep)) +    &
                  INT((tign_g(i,j+1) + ABS(tign_g(i,j+1)) +  2.*ep)/      &
                                   (2.*ABS(tign_g(i,j+1)) + 1.5*ep)) +    &
                  INT((tign_g(i,j-1) + ABS(tign_g(i,j-1)) +  2.*ep)/      &
                                   (2.*ABS(tign_g(i,j-1)) + 1.5*ep)) +    &
                  INT((tign_g(i+1,j+1)+ABS(tign_g(i+1,j+1)) +  2.*ep)/    &
                                   (2.*ABS(tign_g(i+1,j+1)) + 1.5*ep)) +  &
                  INT((tign_g(i-1,j+1)+ABS(tign_g(i-1,j+1)) +  2.*ep)/    &
                                   (2.*ABS(tign_g(i-1,j+1)) + 1.5*ep)) +  &
                  INT((tign_g(i+1,j-1)+ABS(tign_g(i+1,j-1)) +  2.*ep)/    &
                                   (2.*ABS(tign_g(i+1,j-1)) + 1.5*ep)) +  &
                  INT((tign_g(i-1,j-1)+ABS(tign_g(i-1,j-1)) +  2.*ep)/    &
                                   (2.*ABS(tign_g(i-1,j-1)) + 1.5*ep))
            END IF                       ! nfl=1
         END DO
         END DO
 
         DO j = jf_st+1,jf_en-1
         DO i = if_st+1,if_en-1

            IF (nfl(i,j) == 1) THEN     !NFL=1

               isum = islsum(i,j)

               IF (isum == 8) THEN         !ISUM=8

                  itest = 1
 
                  IF (nc(i,j) == 1 .AND. icl(i,j) == 4) THEN        !1/4 treatment
! ----- test for threat of second ignition (itest=0 is necessary to continue)
                  itest=(1-icn(i,j,1))*(icn(i-1,j,2)+icn(i-1,j-1,4)+icn(i,j-1,3)) &
                       +(1-icn(i,j,2))*(icn(i+1,j,1)+icn(i+1,j-1,3)+icn(i,j-1,4)) &
                       +(1-icn(i,j,3))*(icn(i-1,j,4)+icn(i-1,j+1,2)+icn(i,j+1,1)) &
                       +(1-icn(i,j,4))*(icn(i+1,j,3)+icn(i+1,j+1,1)+icn(i,j+1,2))
                  END IF                                             !1/4 treatment
 
                  IF (itest > 0) THEN
!!                   nfl(i,j) = 0
                     area2(i,j) = 1.
                     DO it = 1,4
                        ixb(i,j,it) = 0
                        iyb(i,j,it) = 0
                        icn(i,j,it) = 1
                        xfg(i,j,it) = xlm(it)
                        yfg(i,j,it) = ylm(it)
                     END DO
                  END IF                ! itest > 0
               END IF                   ! isum = 8
            END IF                      ! nfl = 1
         END DO
         END DO
 
         DO j = jf_st+1,jf_en-1
         DO i = if_st+1,if_en-1
            IF (nfl(i,j) == 1) THEN     ! nfl=1
               isum = islsum(i,j)
               IF (isum == 8) THEN      ! isum = 8
                  itest = 1

                  IF (nc(i,j) == 2 .AND. icl(i,j) == 2) THEN     ! 2/2 treatment
                     IF (icn(i,j,1)+icn(i,j,2) == 2) itest=            &
                        (icn(i-1,j,4)+icn(i-1,j+1,2)+icn(i,j+1,1))     &
                      + (icn(i+1,j,3)+icn(i+1,j+1,1)+icn(i,j+1,2))
                     IF (icn(i,j,3)+icn(i,j,4) == 2) itest=            &
                        (icn(i-1,j,2)+icn(i-1,j-1,4)+icn(i,j-1,3))     &
                      + (icn(i+1,j,1)+icn(i+1,j-1,3)+icn(i,j-1,4))
                     IF (icn(i,j,1)+icn(i,j,3) == 2) itest=            &
                        (icn(i+1,j,1)+icn(i+1,j-1,3)+icn(i,j-1,4))     &
                      + (icn(i+1,j,3)+icn(i+1,j+1,1)+icn(i,j+1,2))
                     IF (icn(i,j,2)+icn(i,j,4) == 2) itest=            &
                        (icn(i-1,j,2)+icn(i-1,j-1,4)+icn(i,j-1,3))     &
                      + (icn(i-1,j,4)+icn(i-1,j+1,2)+icn(i,j+1,1))
                  END IF                                          ! 2/2 treatment
 
                  IF (itest > 0) THEN
!                    itot = itot + 1
!!                   nfl(i,j) = 0
                     area2(i,j) = 1.
                     DO it = 1,4
                        ixb(i,j,it) = 0
                        iyb(i,j,it) = 0
                        icn(i,j,it) = 1
                        xfg(i,j,it) = xlm(it)
                        yfg(i,j,it) = ylm(it)
                     END DO
                  END IF                ! itest > 0
               END IF                   ! isum = 8
            END IF                      ! nfl = 1
         END DO
         END DO
 
         DO j = jf_st+1,jf_en-1
         DO i = if_st+1,if_en-1

            IF (nfl(i,j) == 1) THEN     !NFL=1

               isum = islsum(i,j)

               IF (isum == 8) THEN         !ISUM=8

                  itest = 1
 
                  IF (nc(i,j) == 3 .AND. icl(i,j) == 1) THEN      !3/1 treatment

                     ic1 = 10-icn(i,j,1)-2*icn(i,j,2)-3*icn(i,j,3)-4*icn(i,j,4)

                     IF (ic1 == 1) THEN
                        itest = (1-icn(i,j,ic1)) *                       &
                            (icn(i-1,j,2)+icn(i-1,j-1,4) + icn(i,j-1,3)  &
                           + icn(i,j-1,1)*iyb(i,j-1,3) +                 &
                                          icn(i-1,j,1)*ixb(i,j-1,2)      &
                           + icn(i,j-1,2)*icn(i-1,j-1,2)*iyb(i,j,1)      &
                           + icn(i-1,j,3)*icn(i-1,j-1,3)*ixb(i,j,1))
                     END IF
 
                     IF (ic1 == 2) THEN
                        itest = (1-icn(i,j,ic1)) *                       &
                            (icn(i+1,j,1)+icn(i+1,j-1,3)+icn(i,j-1,4)    &
                           + icn(i,j-1,2)*iyb(i,j-1,4)+                  &
                                          icn(i+1,j,2)*ixb(i,j-1,1)      &
                           + icn(i,j-1,1)*icn(i+1,j-1,1)*iyb(i,j,2)      &
                           + icn(i+1,j,4)*icn(i+1,j-1,4)*ixb(i,j,2))
                     END IF

                     IF (ic1 == 3) THEN
                        itest = (1-icn(i,j,ic1)) *                       &
                           (icn(i-1,j,4)+icn(i-1,j+1,2)+icn(i,j+1,1)     &
                          + icn(i,j+1,3)*iyb(i,j+1,1) +                  &
                                         icn(i-1,j,3) * ixb(i-1,j,4)     &
                          + icn(i,j+1,4)*icn(i-1,j+1,4)*iyb(i,j,3)       &
                          + icn(i-1,j,1)*icn(i-1,j+1,1)*ixb(i,j,3))      
                     END IF

                     IF (ic1 == 4) THEN
                        itest = (1-icn(i,j,ic1)) *                       &
                          (icn(i+1,j,3)+icn(i+1,j+1,1)+icn(i,j+1,2)      &
                          + icn(i,j+1,4)*iyb(i,j+1,2)+                   &
                                         icn(i+1,j,4)*ixb(i+1,j,3)       &
                          + icn(i,j+1,3)*icn(i+1,j+1,3)*iyb(i,j,4)       &
                          + icn(i+1,j,2)*icn(i+1,j+1,2)*ixb(i,j,4))
                     END IF

                  END IF                                           !3/1 treatment
 
                  IF (itest > 0) THEN
                     area2(i,j) = 1.
                     DO it = 1,4
                        ixb(i,j,it) = 0
                        iyb(i,j,it) = 0
                        icn(i,j,it) = 1
                        xfg(i,j,it) = xlm(it)
                        yfg(i,j,it) = ylm(it)
                     END DO
                  END IF                ! itest > 0
               END IF                   ! ISUM = 8
            END IF                      ! NFL = 1
         END DO
         END DO
 
  ! --- here we are figuring out which cells define the fire line
  !
  !  if tign_g() > 0 then nfl() is set to 1 (this only tells us
  !  if the cell is on fire.  then we go find out whether our
  !  neighbors are on fire and whether their tracers are in the
  !  adjacent corner.  if all four neighboring cells are on fire
  !  and the tracers are in the corners nearest my current point,
  !  then we must be in an interior point, nfl()=0.  if all four 
  !  neighboring cells don't have tracers pushed to the corner near 
  !  me, then we must be at the edge of the fire, nfl() = 1.

         DO j = jf_st,jf_en
         DO i = if_st,if_en
            nfl(i,j) = INT( (ep + tign_g(i,j) + ABS(ep + tign_g(i,j))) /   &
                                     (2.*ABS(tign_g(i,j))+2.*ep)+.5 )
            nfl_t(i,j) = (1 - INT(area(i,j) + ep))   !test
         END DO
         END DO

         DO j = jf_st+1,jf_en-1
         DO i = if_st+1,if_en-1
            nfl_t(i,j) = (1 - int(area(i,j) + ep - 2.*ep*                &
               (1. - FLOAT((                                             &
                       ((icn(i+1,j,1) + icn(i+1,j,3))/2)*nfl(i+1,j) +    &
                       ((icn(i-1,j,2) + icn(i-1,j,4))/2)*nfl(i-1,j) +    &
                       ((icn(i,j+1,1) + icn(i,j+1,2))/2)*nfl(i,j+1) +    &
                       ((icn(i,j-1,3) + icn(i,j-1,4))/2)*nfl(i,j-1)      &
                       )/4))))         !test
         END DO
         END DO

         DO j = jf_st,jf_en
         DO i = if_st,if_en
            nfl(i,j) = nfl(i,j)*nfl_t(i,j)
         END DO
         END DO
 
        ! --- check on validity of 1/4's and 2/2's

         DO j = jf_st+1,jf_en-1
         DO i = if_st+1,if_en-1
            IF (nfl(i,j) == 1) THEN    !NFL=1
               nct  = icn(i,j,1)+icn(i,j,2)+icn(i,j,3)+icn(i,j,4)
               icls = ixb(i,j,1)+ixb(i,j,2)+ixb(i,j,3)+ixb(i,j,4) +  &
                      iyb(i,j,1)+iyb(i,j,2)+iyb(i,j,3)+iyb(i,j,4)         

               ! --- check that 1/4 is logical

               IF (nct == 1 .AND. icls == 4) THEN  ! nct=1 icls=4 validity test

                  CALL fire_valid14(i,j,ic1,ic2,ic3,ic4,ita,itb,itc,nc, &
                                    icl,xfg,yfg,ixb,iyb,icn,            &
                                    ifms,ifme, kfms,kfme, jfms,jfme)

                  IF (ita == 0 .AND. itb == 0 .AND. itc > 0) THEN
                     xfg(i,j,ic3) = xlm(ic3)
                     yfg(i,j,ic3) = ylm(ic3)
                     icn(i,j,ic3) = 1
                     ixb(i,j,ic3) = 0
                     iyb(i,j,ic3) = 0
                     xfg(i,j,ic4) = xlm(ic4)
                     yfg(i,j,ic4) = ylm(ic4)
                     icn(i,j,ic4) = 1
                     ixb(i,j,ic4) = 0
                     iyb(i,j,ic4) = 0

            ! ----- following call takes care of indexing order effects

                     IF (nc(i-1,j) == 1 .AND. icl(i-1,j) == 4 .AND. i > 2)  &
                        CALL fire_valid14(i-1,j,ic1,ic2,ic3,ic4,ita,itb,itc,nc, &
                                         icl,xfg,yfg,ixb,iyb,icn,               &
                                         ifms,ifme, kfms,kfme, jfms,jfme)
                     IF (nc(i,j-1) == 1 .AND. icl(i,j-1) == 4 .AND. j > 2)  &
                        CALL fire_valid14(i,j-1,ic1,ic2,ic3,ic4,ita,itb,itc,nc, &
                                         icl,xfg,yfg,ixb,iyb,icn,               &
                                         ifms,ifme, kfms,kfme, jfms,jfme)
                  END IF
               END IF                  ! NCT=1 ICLS=4 validity test
            END IF                     ! NFL=1
         END DO
         END DO
 
         DO j = jf_st+1,jf_en-1
         DO i = if_st+1,if_en-1
            IF (nfl(i,j) == 1) THEN    !NFL=1
               nct  = icn(i,j,1) + icn(i,j,2) + icn(i,j,3) + icn(i,j,4)     
               icls = ixb(i,j,1) + ixb(i,j,2) + ixb(i,j,3) + ixb(i,j,4) + &
                      iyb(i,j,1) + iyb(i,j,2) + iyb(i,j,3) + iyb(i,j,4)         

               ! -- check on validity of 2/2's

               IF (nct == 2 .AND. icls == 2) THEN   !2/2 TESTING FOLLOWS
                  isum = nfl(i+1,j) + nfl(i-1,j) 
 
                  IF (iyb(i,j,1)+iyb(i,j,2) == 2 .AND. isum < 2) THEN
                     IF (nfl(i+1,j) == 0 .AND. area2(i+1,j) > 1.-ep) THEN
                        yfg(i,j,2) = ylm(2)
                     END IF
                     IF (nfl(i-1,j) == 0 .AND. area2(i-1,j) > 1.-ep) THEN
                        yfg(i,j,1) = ylm(1)
                     END IF
                  END IF
 
                  IF (iyb(i,j,3)+iyb(i,j,4) == 2 .AND. isum < 2) THEN
                     IF (nfl(i+1,j) == 0 .AND. area2(i+1,j) > 1.-ep) THEN
                        yfg(i,j,4) = ylm(4)
                     END IF
                     IF (nfl(i-1,j) == 0 .AND. area2(i-1,j) > 1.-ep) THEN
                        yfg(i,j,3) = ylm(3)
                     END IF
                  END IF
 
                  jsum = nfl(i,j+1) + nfl(i,j-1)
                  IF (ixb(i,j,1)+ixb(i,j,3) == 2 .AND. jsum < 2) THEN
                     IF (nfl(i,j+1) == 0 .AND. area2(i,j+1) > 1.-ep) THEN
                        xfg(i,j,3) = xlm(3)
                     END IF
                     IF (nfl(i,j-1) == 0 .AND. area2(i,j-1) > 1.-ep) THEN
                        xfg(i,j,1) = xlm(1)
                     END IF
                  END IF
 
                  IF (ixb(i,j,2)+ixb(i,j,4) == 2 .AND. jsum < 2) THEN
                     IF (nfl(i,j+1) == 0 .AND. area2(i,j+1) > 1.-ep) THEN
                        xfg(i,j,4) = xlm(4)
                     END IF
                     IF (nfl(i,j-1) == 0 .AND. area2(i,j-1) > 1.-ep) THEN
                        xfg(i,j,2) = xlm(2)
                     END IF
                  END IF
               END IF                             !2/2 TESTING ABOVE
 
              ! --- this portion of code was not active in the dand1 testing
              !     debug new code follows
               IF (nct == 3 .AND. (icls == 1 .OR. icls == 2)) THEN   ! 3/1 TESTING
                  ic2 = (1-icn(i,j,1)) + 2*(1-icn(i,j,2))   &
                    + 3*(1-icn(i,j,3)) + 4*(1-icn(i,j,4))
                  ic1 = 5 - ic2
                  ic3 = 2 - (ic1*ic2)/6
                  ic4 = 10 - ic1 - ic2 - ic3
                  t1 = ABS(ABS(xfg(i,j,ic2))-.5) + ABS(ABS(yfg(i,j,ic2))-.5)
                  IF (t1 < 10.*ep) THEN
                     WRITE(msg,*) 'debug 3/1 to 3/2 conversion hit' 
                     call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                     xfg(i,j,ic2) = .5*(xfg(i,j,ic3)+xfg(i,j,ic4))
                     yfg(i,j,ic2) = .5*(yfg(i,j,ic3)+yfg(i,j,ic4))
                     ixb(i,j,ic2) = 1
                     iyb(i,j,ic2) = 1
                  END IF
               END IF                 !3/1 TESTING FOLLOWS   
               !     debug new code above
               ! --- the above portion of code was not active in the dand1 testing
 
            END IF    !NFL=1
         END DO
         END DO

         DO j = jf_st,jf_en
         DO i = if_st,if_en
            nc(i,j)  = icn(i,j,1)+icn(i,j,2)+icn(i,j,3)+icn(i,j,4)
            icl(i,j) = ixb(i,j,1)+ixb(i,j,2)+ixb(i,j,3)+ixb(i,j,4) +  &
                       iyb(i,j,1)+iyb(i,j,2)+iyb(i,j,3)+iyb(i,j,4)         
         END DO
         END DO
 
         DO j = jf_st+1,jf_en-1
         DO i = if_st+1,if_en-1

            IF (nfl(i,j) == 1) THEN       !NFL=1

               nct = nc(i,j)
               icls = icl(i,j)

               ! --- align neighbors with 2/2s

               IF (nct == 2 .AND. icls == 2) THEN   !NCT=2 ICLS=2
 
                  IF (icn(i,j,1)+icn(i,j,3) == 2) THEN
                     IF (nfl(i,j+1)*ixb(i,j+1,2) == 1  &
                                    .AND. iyb(i,j+1,2) == 0) THEN
                        xavg = .5*(xfg(i,j+1,2)+xfg(i,j,4))
!                       xavg = amax1(xfg(i,j+1,2),xfg(i,j,4))
                        xfg(i,j+1,2) = xavg
                        xfg(i,j,4) = xavg
                     END IF
                     IF (nc(i,j+1) == 4) xfg(i,j,4) = xlm(4)
                     IF (nfl(i,j-1)*ixb(i,j-1,4) == 1  &
                                    .AND. iyb(i,j-1,4) == 0) THEN
                        xavg = .5*(xfg(i,j-1,4)+xfg(i,j,2))
!                       xavg = amax1(xfg(i,j-1,4),xfg(i,j,2))
                        xfg(i,j-1,4) = xavg
                        xfg(i,j,2) = xavg
                     END IF
                     IF (nc(i,j-1) == 4) xfg(i,j,2) = xlm(2)
                  END IF
 
                  IF (icn(i,j,2)+icn(i,j,4) == 2) THEN
                     IF (nfl(i,j+1)*ixb(i,j+1,1) == 1  &
                                    .AND. iyb(i,j+1,1) == 0) THEN
                        xavg = .5*(xfg(i,j+1,1)+xfg(i,j,3))
!                       xavg = MIN(xfg(i,j+1,1),xfg(i,j,3))
                        xfg(i,j+1,1) = xavg
                        xfg(i,j,3) = xavg
                     END IF
                     IF (nc(i,j+1) == 4) xfg(i,j,3) = xlm(3)
                     IF (nfl(i,j-1)*ixb(i,j-1,3) == 1  &
                                    .AND. iyb(i,j-1,3) == 0) THEN
                        xavg = .5*(xfg(i,j-1,3)+xfg(i,j,1))
!                       xavg = MIN(xfg(i,j-1,3),xfg(i,j,1))
                        xfg(i,j-1,3) = xavg
                        xfg(i,j,1) = xavg
                     END IF
                     IF (nc(i,j-1) == 4) xfg(i,j,1) = xlm(1)
                  END IF

                  IF (icn(i,j,1)+icn(i,j,2) == 2) THEN
                     IF (nfl(i+1,j)*iyb(i+1,j,3) == 1  &
                                    .AND. ixb(i+1,j,3) == 0) THEN
                        yavg = .5*(yfg(i+1,j,3)+yfg(i,j,4))
!                       yavg = MAX(yfg(i+1,j,3),yfg(i,j,4))
                        yfg(i+1,j,3) = yavg
                        yfg(i,j,4) = yavg
                     END IF
                     IF (nc(i+1,j) == 4) yfg(i,j,4) = ylm(4)
                     IF (nfl(i-1,j)*iyb(i-1,j,4) == 1  &
                                    .AND. ixb(i-1,j,4) == 0) THEN
                        yavg = .5*(yfg(i-1,j,4)+yfg(i,j,3))
!                       yavg = MAX(yfg(i-1,j,4),yfg(i,j,3))
                        yfg(i-1,j,4) = yavg
                        yfg(i,j,3) = yavg
                     END IF
                     IF (nc(i-1,j) == 4) yfg(i,j,3) = ylm(3)
                  END IF

                  IF (icn(i,j,3)+icn(i,j,4) == 2) THEN
                     IF (nfl(i+1,j)*iyb(i+1,j,1) == 1  &
                                    .AND. ixb(i+1,j,1) == 0) THEN
                        yavg = .5*(yfg(i+1,j,1)+yfg(i,j,2))
!                       yavg = MIN(yfg(i+1,j,1),yfg(i,j,2))
                        yfg(i+1,j,1) = yavg
                        yfg(i,j,2) = yavg
                     END IF
                     IF (nc(i+1,j) == 4) yfg(i,j,2) = ylm(2)
                     IF (nfl(i-1,j)*iyb(i-1,j,2) == 1  &
                                    .AND. ixb(i-1,j,2) == 0) THEN
                        yavg = .5*(yfg(i-1,j,2)+yfg(i,j,1))
!                       yavg = MIN(yfg(i-1,j,2),yfg(i,j,1))
                        yfg(i-1,j,2) = yavg
                        yfg(i,j,1) = yavg
                     END IF
                     IF (nc(i-1,j) == 4) yfg(i,j,1) = ylm(1)
                  END IF

               END IF                             !NCT=2 ICLS=2
 
               ! --- align 1/4 neighbors with 3/1 moving coordinate

               IF (nct == 3 .AND. icls == 1) THEN   !NCT=3 ICLS=1
 
                  IF (icn(i,j,1) == 0) THEN
                     IF(iyb(i,j,1) == 1  &
                                    .AND. nc(i-1,j) == 1  &
                                    .AND. icl(i-1,j) == 4) THEN
                        yfg(i-1,j,2) = yfg(i,j,1) 
                     END IF
                     IF (ixb(i,j,1) == 1  &
                                    .AND. nc(i,j-1) == 1  &
                                    .AND. icl(i,j-1) == 4) THEN
                        xfg(i,j-1,3) = xfg(i,j,1) 
                     END IF
                  END IF

                  IF (icn(i,j,2) == 0) THEN
                     IF (iyb(i,j,2) == 1  &
                                    .AND. nc(i+1,j) == 1  &
                                    .AND. icl(i+1,j) == 4) THEN
                        yfg(i+1,j,1) = yfg(i,j,2) 
                     END IF
                     IF (ixb(i,j,2) == 1  &
                                    .AND. nc(i,j-1) == 1  &
                                    .AND. icl(i,j-1) == 4) THEN
                        xfg(i,j-1,4) = xfg(i,j,2) 
                     END IF
                  END IF
 
                  IF (icn(i,j,3) == 0) THEN
                     IF (iyb(i,j,3) == 1  &
                                    .AND. nc(i-1,j) == 1  &
                                    .AND. icl(i-1,j) == 4) THEN
                        yfg(i-1,j,4) = yfg(i,j,3) 
                     END IF
                     IF (ixb(i,j,3) == 1  &
                                    .AND. nc(i,j+1) == 1  &
                                    .AND. icl(i,j+1) == 4) THEN
                        xfg(i,j+1,1) = xfg(i,j,3) 
                     END IF
                  END IF
 
                  IF (icn(i,j,4) == 0) THEN
                     IF (iyb(i,j,4) == 1  &
                                    .AND. nc(i+1,j) == 1  &
                                    .AND. icl(i+1,j) == 4) THEN
                        yfg(i+1,j,3) = yfg(i,j,4) 
                     END IF
                     IF (ixb(i,j,4) == 1  &
                                    .AND. nc(i,j+1) == 1  &
                                    .AND. icl(i,j+1) == 4) THEN
                        xfg(i,j+1,2) = xfg(i,j,4) 
                     END IF
                  END IF

               END IF                             !NCT=3 ICLS=1
 
               ! --- align abutting 1/4's

               itest=1
               if (itest.eq.1) then
                  IF (nct.eq.1 .AND. icls == 4) THEN                 !NCT=1 ICLS=4
                     ic1 =icn(i,j,1) + 2*icn(i,j,2) + 3*icn(i,j,3) + 4*icn(i,j,4)
                     ic2 = 5 - ic1
                     ic3 = ixb(i,j,1)*(1-iyb(i,j,1))+2*ixb(i,j,2)*(1-iyb(i,j,2)) + &
                         3*ixb(i,j,3)*(1-iyb(i,j,3))+4*ixb(i,j,4)*(1-iyb(i,j,4))
                     ic4 = 10 - ic1 - ic2 - ic3
                     iod = ic1 - 2*(ic1/2)
                     is = 1 - 2*iod
                     js = -1 + 2*(ic1/3)

                     IF (ic1 >= 1) THEN
                        IF (nc(i,j+js) == 1  .AND.     &
                            icl(i,j+js) == 4 .AND.     &
                            icn(i,j+js,ic4) == 1) THEN
                           xfg_a = .5*(xfg(i,j,ic3)+xfg(i,j+js,ic2))
                           xfg(i,j,ic3) = xfg_a
                           xfg(i,j+js,ic2) = xfg_a
                   ! ----- align central coordinate
                           xfg(i,j,ic2) = .5*(xfg(i,j,ic3)+xfg(i,j,ic4)) 
!                          yfg(i,j,ic2) = .5*(yfg(i,j,ic3)+yfg(i,j,ic4))
                        END IF
                        IF (nc(i+is,j).eq.1 .AND. &
                            icl(i+is,j).eq.4 .AND. &
                            icn(i+is,j,ic3) == 1) THEN
                           yfg_a = .5*(yfg(i,j,ic4)+yfg(i+is,j,ic2))
                           yfg(i,j,ic4) = yfg_a
                           yfg(i+is,j,ic2) = yfg_a
                   ! ----- align central coordinate
!                          xfg(i,j,ic2) = .5*(xfg(i,j,ic3)+xfg(i,j,ic4))
                           yfg(i,j,ic2) = .5*(yfg(i,j,ic3)+yfg(i,j,ic4))
                        END IF
                     END IF

                  END IF                  !NCT=1 ICLS=4
               END IF
            END IF                        !NFL=1
         END DO
         END DO
 
         DO j = jf_st,jf_en
         DO i = if_st,if_en
            tmp(i,j) = .5*((xfg(i,j,4)-xfg(i,j,1))*(yfg(i,j,3)-yfg(i,j,2))    &
                         + (yfg(i,j,4)-yfg(i,j,1))*(xfg(i,j,2)-xfg(i,j,3)))
            area2(i,j) = tmp(i,j)
            If (nfl(i,j) < 0 .AND. ABS(time-tign_g(i,j)) < dt) THEN
               tmp(i,j) = 0.0
               nfl(i,j) = 0
               tign_g(i,j) = -100.
               DO it = 1,4
                  xfg(i,j,it) = 0.0
                  yfg(i,j,it) = 0.0
               END DO
            END IF
         END DO
         END DO
 
    ! --- identify cells that are completely on fire for fire_burn_fcn

         DO j = jf_st+1,jf_en-1
         DO i = if_st+1,if_en-1
            IF (area2(i,j) > (1.-ep) .AND. tign_crt(i,j) < 0.) THEN
                tign_crt(i,j) = time
            END IF
         END DO
         END DO

      END IF              ! iffg == 2
   END IF                 ! iffg > 0
 
   RETURN

END SUBROUTINE fire_stat

! =========================================================================

SUBROUTINE fire_ln(dt,time,sprdx,sprdy,              & ! in
                   ifms,ifme, kfms,kfme, jfms,jfme,     &
                   if_st,if_en,jf_st,jf_en,             &
                   ncod,in1,in2,ixb,iyb,icn,            & ! inout
                   tign_g,tign_crt,area,area2,xfg,yfg,  &
                   nfl,nfl_t,radhld,xcd,ycd,xcn,ycn)      ! out

! ----- this routine creates points outlining the fire

   IMPLICIT NONE

! ----- incoming variables

   INTEGER, INTENT(in) :: ifms,ifme, kfms,kfme, jfms,jfme
   INTEGER, INTENT(in) :: if_st,if_en, jf_st,jf_en

   REAL, INTENT(in)    :: dt,time

   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 )    :: sprdx,sprdy

! ----- inout variables

   INTEGER, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: ncod
   INTEGER, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,2 ) :: in1,in2
   INTEGER, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: ixb,iyb,icn

   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )    :: tign_g,tign_crt
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )    :: area,area2
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 )  :: xfg,yfg

! ----- outgoing variables

   INTEGER, INTENT(out), DIMENSION( ifms:ifme,jfms:jfme ) :: nfl,nfl_t

   REAL, INTENT(out) :: radhld

   REAL, INTENT(out), DIMENSION( ifms:ifme,jfms:jfme,4 )    :: xcd,ycd
   REAL, INTENT(out), DIMENSION( ifms:ifme,jfms:jfme,4 )    :: xcn,ycn

! ----- local variables

   INTEGER :: i,j,it
   INTEGER :: istat
   INTEGER :: nct,icls
   INTEGER :: ic1,ic2,ic3,ic4
   INTEGER :: i1,i2,i3
   INTEGER :: j1,j2,j3
   INTEGER :: iflt
   INTEGER :: inxt,inyt
   INTEGER :: iod
   INTEGER :: is,js
   INTEGER :: isen
   INTEGER :: it1,it2,it3,it4
   INTEGER :: nh0,nh1,nh2,nh3,nht
   INTEGER :: itt
   INTEGER :: ihit
   INTEGER :: i1tst,j1tst
   INTEGER :: i2tst,j2tst
   INTEGER :: nh11,nh01,nh10,nhtmp
   INTEGER :: ilm1,ilm2,ilm3,ilm4
   INTEGER :: itest
   INTEGER :: iloc
   INTEGER :: ihld,jhld

   INTEGER, DIMENSION( ifms:ifme,jfms:jfme ) :: nc,icl

   REAL :: t1
   REAL :: tlx,tly
   REAL :: t1tst
   REAL :: x02,y02,r02
   REAL :: x20,y20,r20
   REAL :: x22,y22,r22
   REAL :: x01,y01,r01
   REAL :: x10,y10,r10
   REAL :: x11,y11,r11
   REAL :: r1sq,r2sq
   REAL :: radmax,radmin,radavg,radsum,radtst
   REAL :: x1,y1
   REAL :: x2,y2
   REAL :: x3,y3
   REAL :: det,aa,bb,x_0,y_0,rad,vtsgn

   CHARACTER(LEN=256) :: msg

! ----- when deriving fireline coordinates we always keep the fire to our left

    call wrf_debug(_FR_DBG_LEVEL_,'in fire_ln: 1')
 
! -----   calculate nfl(i,j)

    DO j = jf_st,jf_en
    DO i = if_st,if_en
       nfl(i,j) = INT( (ep+tign_g(i,j) + ABS(ep+tign_g(i,j)))  &
                        / (2.*ABS(tign_g(i,j))+2.*ep) + .5 )
       nfl_t(i,j) = (1 - INT(area(i,j) + ep))   !test
    END DO
    END DO

    call wrf_debug(_FR_DBG_LEVEL_,'in fire_ln: 2')
    DO j = jf_st+1,jf_en-1
    DO i = if_st+1,if_en-1
       nfl_t(i,j) = (1-INT(area(i,j)+ep-2.*ep*               &
          (1.-FLOAT((                                        &
          ((icn(i+1,j,1)+icn(i+1,j,3))/2)*nfl(i+1,j) +       &
          ((icn(i-1,j,2)+icn(i-1,j,4))/2)*nfl(i-1,j) +       &
          ((icn(i,j+1,1)+icn(i,j+1,2))/2)*nfl(i,j+1) +       &
          ((icn(i,j-1,3)+icn(i,j-1,4))/2)*nfl(i,j-1)         &
          )/4))))         !test
    END DO
    END DO
    call wrf_debug(_FR_DBG_LEVEL_,'in fire_ln: 3')

    DO j = jf_st,jf_en
    DO i = if_st,if_en
       nfl(i,j) = nfl(i,j) * nfl_t(i,j)
       DO it = 1,4
          xcd(i,j,it) = 0.0
          ycd(i,j,it) = 0.0
          xcn(i,j,it) = 0.0
          ycn(i,j,it) = 0.0
       END DO
    END DO
    END DO

    call wrf_debug(_FR_DBG_LEVEL_,'in fire_ln: 4')
    CALL fire_stat(0,dt,time,                       & ! send
                   xcd,ycd,xcn,ycn,sprdx,sprdy,  &
                   ifms,ifme, kfms,kfme, jfms,jfme, &
                   if_st,if_en,jf_st,jf_en,         &
                   nfl,nfl_t,tign_g,tign_crt,       & ! send&recv
                   area,area2,xfg,yfg,              &
                   ixb,iyb,icn)                       ! recv
 
    call wrf_debug(_FR_DBG_LEVEL_,'in fire_ln: 5')
    DO j = jf_st,jf_en
    DO i = if_st,if_en
       icl(i,j) = ixb(i,j,1) + ixb(i,j,2) + ixb(i,j,3) + ixb(i,j,4) +   &
                  iyb(i,j,1) + iyb(i,j,2) + iyb(i,j,3) + iyb(i,j,4)         
       nc(i,j)  = icn(i,j,1) + icn(i,j,2) + icn(i,j,3) + icn(i,j,4)
    END DO
    END DO
    call wrf_debug(_FR_DBG_LEVEL_,'in fire_ln: 6')
 
! ----- get xcd,ycd
 
y_cd: DO j = jf_st+1,jf_en-1
x_cd: DO i = if_st+1,if_en-1

       IF (nfl(i,j) == 1) THEN   !NFL outer loop

          istat = 0
          nct = nc(i,j)
 
          IF (nct == 0) THEN                    !NCT=0
             iflt = 1
             CALL fire_error_debug(i,j,iflt,          &
                     time,in1,in2,tign_g,             &
                     nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                     ixb,iyb,xfg,yfg,                 &
                     ifms,ifme, kfms,kfme, jfms,jfme, &
                     if_st,if_en,jf_st,jf_en)
          END IF                                !NCT=0
 
          ncod(i,j) = 0
          icls = icl(i,j)
 
          IF (nct == 4) THEN                    !NCT=4 ICLS=0
             ncod(i,j) = 2
             IF (tign_g(i+1,j) < 0.0 .OR.                                &
                   nfl(i+1,j)*(icn(i+1,j,1)+icn(i+1,j,3)) < 2) THEN
                ycd(i,j,1) = (1-icn(i+1,j,1))*yfg(i,j,2) + icn(i+1,j,1)*  &
                   yfg(i+1,j,3)
                ycd(i,j,2) = (1-icn(i+1,j,3))*yfg(i,j,4) + icn(i+1,j,3)*  &
                   yfg(i+1,j,1)
                xcd(i,j,1) = xlm(2)
                xcd(i,j,2) = xlm(4)
                istat = istat + 1
             END IF
 
             IF (tign_g(i-1,j) < 0.0 .OR.                                &
                   nfl(i-1,j)*(icn(i-1,j,2)+icn(i-1,j,4)) < 2) THEN
                ycd(i,j,1) = (1-icn(i-1,j,4))*yfg(i,j,3) + icn(i-1,j,4)*  &
                   yfg(i-1,j,2)
                ycd(i,j,2) = (1-icn(i-1,j,2))*yfg(i,j,1) + icn(i-1,j,2)*  &
                   yfg(i-1,j,4)
                xcd(i,j,1) = xlm(3)
                xcd(i,j,2) = xlm(1)
                istat = istat + 1
             END IF
 
             IF (tign_g(i,j+1) < 0.0 .OR.                                &
                   nfl(i,j+1)*(icn(i,j+1,1)+icn(i,j+1,2)) < 2) THEN
                xcd(i,j,1) = (1-icn(i,j+1,2))*xfg(i,j,4) + icn(i,j+1,2)*  &
                   xfg(i,j+1,1)
                xcd(i,j,2) = (1-icn(i,j+1,1))*xfg(i,j,3) + icn(i,j+1,1)*  &
                   xfg(i,j+1,2)
                ycd(i,j,1) = ylm(4)
                ycd(i,j,2) = ylm(3)
                istat = istat + 1
             END IF
 
             IF (tign_g(i,j-1) < 0.0 .OR.                                &
                   nfl(i,j-1)*(icn(i,j-1,3)+icn(i,j-1,4)) < 2) THEN
                xcd(i,j,1) = (1-icn(i,j-1,3))*xfg(i,j,1)+icn(i,j-1,3)*    &
                   xfg(i,j+1,4)
                xcd(i,j,2) = (1-icn(i,j-1,4))*xfg(i,j,2)+icn(i,j-1,4)*    &
                   xfg(i,j+1,3)
                ycd(i,j,1) = ylm(1)
                ycd(i,j,2) = ylm(2)
                istat = istat + 1
             END IF
          END IF                                !NCT=4 ICLS=0
 
          IF (nct == 3 .AND. icls == 1) THEN            !NCT=3 AND ICLS=1
             ncod(i,j) = 2
             istat = istat + 1
             ic1 = (1-icn(i,j,1)) + 2*(1-icn(i,j,2)) +              &
                 3*(1-icn(i,j,3)) + 4*(1-icn(i,j,4))
             ic3 = (1-icn(i,j,1))*(2+ixb(i,j,1))                    &
                  + (1-icn(i,j,2))*(1+3*ixb(i,j,2))                 &
                  + (1-icn(i,j,3))*(4-3*ixb(i,j,3))                 &
                  + (1-icn(i,j,4))*(3-ixb(i,j,4)) 
             i1 = ((ic1-2)*(ic1-3)*(1+ixb(i,j,ic1))                 &
                   + (ic1-1)*(4-ic1)*(2-ixb(i,j,ic1)))/2
             i2 = 3 - i1
! ------------------------------- inxt =0 means no virtual 1=virtual x-coordinate
             inxt = (1-ixb(i,j,ic1))*(                              & 
                (1-icn(i,j,1))*(1-iyb(i,j-1,3)) +                   &
                (1-icn(i,j,2))*(1-iyb(i,j-1,4)) +                   &
                (1-icn(i,j,3))*(1-iyb(i,j+1,1)) +                   &
                (1-icn(i,j,4))*(1-iyb(i,j+1,2)))
! ------------------------------- inyt =0 means no virtual 1=virtual y-coordinate
             inyt = ixb(i,j,ic1)*(                                  &
                (1-icn(i,j,1))*(1-ixb(i-1,j,2)) +                   &
                (1-icn(i,j,2))*(1-ixb(i+1,j,1)) +                   &
                (1-icn(i,j,3))*(1-ixb(i-1,j,4)) +                   &
                (1-icn(i,j,4))*(1-ixb(i+1,j,3)))
             xcd(i,j,i1) = xfg(i,j,ic1)
             ycd(i,j,i1) = yfg(i,j,ic1)
             xcd(i,j,i2) = FLOAT(1-inxt)*xlm(ic3) + FLOAT(inxt)*(   &
                FLOAT(1-icn(i,j,1))*xfg(i,j-1,3)+                   &
                FLOAT(1-icn(i,j,2))*xfg(i,j-1,4)+                   &
                FLOAT(1-icn(i,j,3))*xfg(i,j+1,1)+                   &
                FLOAT(1-icn(i,j,4))*xfg(i,j+1,2))
             ycd(i,j,i2) = FLOAT(1-inyt)*ylm(ic3) + FLOAT(inyt)*(   &
                FLOAT(1-icn(i,j,1))*yfg(i-1,j,2)+                   &
                FLOAT(1-icn(i,j,2))*yfg(i+1,j,1)+                   &
                FLOAT(1-icn(i,j,3))*yfg(i-1,j,4)+                   &
                FLOAT(1-icn(i,j,4))*yfg(i+1,j,3))
          END IF                                    !NCT=3 AND ICLS=1
 
          IF (nct == 3 .AND. icls == 2) THEN        !NCT=3 AND ICLS=2
 
             ncod(i,j) = 3
 
             IF (icn(i,j,1) == 0) THEN
                xcd(i,j,1) = xfg(i,j,3)
                xcd(i,j,2) = xfg(i,j,1)
                xcd(i,j,3) = FLOAT(1-nfl(i,j-1)*ixb(i,j-1,3))*xfg(i,j,2)  &
                   + FLOAT(nfl(i,j-1)*ixb(i,j-1,3))*xfg(i,j-1,3)
                ycd(i,j,1) = FLOAT(1-nfl(i-1,j)*iyb(i-1,j,2))*yfg(i,j,3)  &
                   + FLOAT(nfl(i-1,j)*iyb(i-1,j,2))*yfg(i-1,j,2)
                ycd(i,j,2) = yfg(i,j,1)
                ycd(i,j,3) = yfg(i,j,2)
                istat = istat + 1
             END IF
 
             IF (icn(i,j,2) == 0) THEN
                xcd(i,j,1) = FLOAT(1-nfl(i,j-1)*ixb(i,j-1,4))*xfg(i,j,1)    &
                           + FLOAT(  nfl(i,j-1)*ixb(i,j-1,4))*xfg(i,j-1,4)
                xcd(i,j,2) = xfg(i,j,2)
                xcd(i,j,3) = xfg(i,j,4)
                ycd(i,j,1) = yfg(i,j,1)
                ycd(i,j,2) = yfg(i,j,2)
                ycd(i,j,3) = FLOAT(1-nfl(i+1,j)*iyb(i+1,j,1))*yfg(i,j,4)    &
                           + FLOAT(  nfl(i+1,j)*iyb(i+1,j,1))*yfg(i+1,j,1)
                istat = istat + 1
             END IF
 
             IF (icn(i,j,3) == 0) THEN
                xcd(i,j,1) = FLOAT(1-nfl(i,j+1)*ixb(i,j+1,1))*xfg(i,j,4)     &
                           + FLOAT(  nfl(i,j+1)*ixb(i,j+1,1))*xfg(i,j+1,1)
                xcd(i,j,2) = xfg(i,j,3)
                xcd(i,j,3) = xfg(i,j,1)
                ycd(i,j,1) = yfg(i,j,4)
                ycd(i,j,2) = yfg(i,j,3)
                ycd(i,j,3) = FLOAT(1-nfl(i-1,j)*iyb(i-1,j,4))*yfg(i,j,1)     &
                           + FLOAT(  nfl(i-1,j)*iyb(i-1,j,4))*yfg(i-1,j,4)
                istat = istat + 1
             END IF
 
             IF (icn(i,j,4) == 0) THEN
                xcd(i,j,1) = xfg(i,j,2)
                xcd(i,j,2) = xfg(i,j,4)
                xcd(i,j,3) = FLOAT(1-nfl(i,j+1)*ixb(i,j+1,2))*xfg(i,j,3)     &
                           + FLOAT(  nfl(i,j+1)*ixb(i,j+1,2))*xfg(i,j+1,2)
                ycd(i,j,1) = FLOAT(1-nfl(i+1,j)*iyb(i+1,j,3))*yfg(i,j,2)     &
                           + FLOAT(  nfl(i+1,j)*iyb(i+1,j,3))*yfg(i+1,j,3)
                ycd(i,j,2) = yfg(i,j,4)
                ycd(i,j,3) = yfg(i,j,3)
                istat = istat + 1
             END IF
 
          END IF                    !NCT=3 AND ICLS=2
 
          IF (nct == 2 .AND. icls == 2) THEN       !NCT=2 ICLS=2
             ncod(i,j) = 2
             IF (icn(i,j,1)+icn(i,j,2) == 2) THEN  !IT=1 AND 2
                xcd(i,j,1) = xfg(i,j,4)
                xcd(i,j,2) = xfg(i,j,3)
                ycd(i,j,1) = yfg(i,j,4)
                ycd(i,j,2) = yfg(i,j,3)
                istat = istat + 1
             END IF                                 !IT=1 AND 2
             IF (icn(i,j,2)+icn(i,j,4) == 2) THEN  !IT=2 AND 4
                xcd(i,j,1) = xfg(i,j,3)
                xcd(i,j,2) = xfg(i,j,1)
                ycd(i,j,1) = yfg(i,j,3)
                ycd(i,j,2) = yfg(i,j,1)
                istat = istat + 1
             END IF                                 !IT=2 AND 4
             IF (icn(i,j,3)+icn(i,j,4) == 2) THEN  !IT=3 AND 4
                xcd(i,j,1) = xfg(i,j,1)
                xcd(i,j,2) = xfg(i,j,2)
                ycd(i,j,1) = yfg(i,j,1)
                ycd(i,j,2) = yfg(i,j,2)
                istat = istat + 1
             END IF                                 !IT=3 AND 4
             IF (icn(i,j,1)+icn(i,j,3) == 2) THEN  !IT=1 AND 3
                xcd(i,j,1) = xfg(i,j,2)
                xcd(i,j,2) = xfg(i,j,4)
                ycd(i,j,1) = yfg(i,j,2)
                ycd(i,j,2) = yfg(i,j,4)
                istat = istat + 1
             END IF                                 !IT=1 AND 3
          END IF                                    !NCT=2 ICLS=2
 
          IF (nct == 2 .AND. icls == 3) THEN        !NCT=2 ICLS=3
             ncod(i,j) = 3
             IF (icn(i,j,1)+icn(i,j,2) == 2) THEN   !IT=1 AND 2
                IF (ixb(i,j,3) == 1) THEN
                   xcd(i,j,1) = xfg(i,j,4)
                   xcd(i,j,2) = xfg(i,j,3)
                   xcd(i,j,3) = xfg(i,j,1)
                   ycd(i,j,1) = yfg(i,j,4)
                   ycd(i,j,2) = yfg(i,j,3)
                   ycd(i,j,3) = FLOAT(1-nfl(i-1,j)*iyb(i-1,j,4))*yfg(i,j,1)    &
                              + FLOAT(  nfl(i-1,j)*iyb(i-1,j,4))*yfg(i-1,j,4)
                   istat = istat+1
                END IF
                IF (ixb(i,j,3) == 0) THEN
                   xcd(i,j,1) = xfg(i,j,2)
                   xcd(i,j,2) = xfg(i,j,4)
                   xcd(i,j,3) = xfg(i,j,3)
                   ycd(i,j,1) = FLOAT(1-nfl(i+1,j)*iyb(i+1,j,3))*yfg(i,j,2)    &
                              + FLOAT(  nfl(i+1,j)*iyb(i+1,j,3))*yfg(i+1,j,3)
                   ycd(i,j,2) = yfg(i,j,4)
                   ycd(i,j,3) = yfg(i,j,3)
                   istat = istat + 1
                END IF
             END IF                                 !IT=1 AND 2
             IF (icn(i,j,2)+icn(i,j,4) == 2) THEN   !IT=2 AND 4
                IF (iyb(i,j,1) == 1) THEN
                   xcd(i,j,1) = xfg(i,j,3)
                   xcd(i,j,2) = xfg(i,j,1)
                   xcd(i,j,3) = FLOAT(1-nfl(i,j-1)*ixb(i,j-1,3))*xfg(i,j,2)    &
                              + FLOAT(  nfl(i,j-1)*ixb(i,j-1,3))*xfg(i,j-1,3)
                   ycd(i,j,1) = yfg(i,j,3)
                   ycd(i,j,2) = yfg(i,j,1)
                   ycd(i,j,3) = yfg(i,j,2)
                   istat = istat + 1
                END IF
                IF (iyb(i,j,1) == 0) THEN
                   xcd(i,j,1) = FLOAT(1-nfl(i,j+1)*ixb(i,j+1,1))*xfg(i,j,4)    &
                              + FLOAT(  nfl(i,j+1)*ixb(i,j+1,1))*xfg(i,j+1,1)
                   xcd(i,j,2) = xfg(i,j,3)
                   xcd(i,j,3) = xfg(i,j,1)
                   ycd(i,j,1) = yfg(i,j,4)
                   ycd(i,j,2) = yfg(i,j,3)
                   ycd(i,j,3) = yfg(i,j,1)
                   istat = istat + 1
                END IF
             END IF                                 !IT=2 AND 4
             IF (icn(i,j,3)+icn(i,j,4) == 2) THEN   !IT=3 AND 4
                IF (ixb(i,j,2) == 0) THEN
                   xcd(i,j,1) = xfg(i,j,3)
                   xcd(i,j,2) = xfg(i,j,1)
                   xcd(i,j,3) = xfg(i,j,2)
                   ycd(i,j,1) = FLOAT(1-nfl(i-1,j)*iyb(i-1,j,2))*yfg(i,j,3)    &
                              + FLOAT(  nfl(i-1,j)*iyb(i-1,j,2))*yfg(i-1,j,2)
                   ycd(i,j,2) = yfg(i,j,1)
                   ycd(i,j,3) = yfg(i,j,2)
                   istat = istat + 1
                END IF
                IF (ixb(i,j,2) == 1) THEN
                   xcd(i,j,1) = xfg(i,j,1)
                   xcd(i,j,2) = xfg(i,j,2)
                   xcd(i,j,3) = xfg(i,j,4)
                   ycd(i,j,1) = yfg(i,j,1)
                   ycd(i,j,2) = yfg(i,j,2)
                   ycd(i,j,3) = FLOAT(1-nfl(i+1,j)*iyb(i+1,j,1))*yfg(i,j,4)   &
                              + FLOAT(  nfl(i+1,j)*iyb(i+1,j,1))*yfg(i+1,j,1)
                   istat = istat + 1
                END IF
             END IF                                 !IT=3 AND 4
             IF (icn(i,j,1)+icn(i,j,3) == 2) THEN   !IT=1 AND 3
                IF (iyb(i,j,2) == 0) THEN
                   xcd(i,j,1) = xfg(i,j,2)
                   xcd(i,j,2) = xfg(i,j,4)
                   xcd(i,j,3) = FLOAT(1-nfl(i,j+1)*ixb(i,j+1,2))*xfg(i,j,3)  &
                              + FLOAT(  nfl(i,j+1)*ixb(i,j+1,2))*xfg(i,j+1,2)
                   ycd(i,j,1) = yfg(i,j,2)
                   ycd(i,j,2) = yfg(i,j,4)
                   ycd(i,j,3) = yfg(i,j,3)
                   istat = istat + 1
                END IF
                IF (iyb(i,j,2) == 1) THEN
                   xcd(i,j,1) = FLOAT(1-nfl(i,j-1)*ixb(i,j-1,4))*xfg(i,j,1)  &
                              + FLOAT(  nfl(i,j-1)*ixb(i,j-1,4))*xfg(i,j-1,4)
                   xcd(i,j,2) = xfg(i,j,2)
                   xcd(i,j,3) = xfg(i,j,4)
                   ycd(i,j,1) = yfg(i,j,1)
                   ycd(i,j,2) = yfg(i,j,2)
                   ycd(i,j,3) = yfg(i,j,4)
                   istat = istat + 1
                END IF
             END IF                                 !IT=1 AND 3
          END IF                                  !NCT=2 ICLS=3
 
          IF (nct == 1 .AND. icls == 4) THEN        !NCT=1 ICLS=4
             ncod(i,j) = 3
             ic1 = icn(i,j,1) + 2*icn(i,j,2) + 3*icn(i,j,3) + 4*icn(i,j,4)
             ic2 = 5 - ic1
             ic3 = ixb(i,j,1)*(1-iyb(i,j,1)) + 2*ixb(i,j,2)*(1-iyb(i,j,2)) +  &
                 3*ixb(i,j,3)*(1-iyb(i,j,3)) + 4*ixb(i,j,4)*(1-iyb(i,j,4))
             ic4 = 10 - ic1 - ic2 - ic3
             iod = ic1 - 2*(ic1/2)
             is = 1 - 2*iod
             js = -1 + 2*(ic1/3)
             isen = is*js

! -------------------- straight lines for stability
             xfg(i,j,ic2) = .5*(xfg(i,j,ic3)+xfg(i,j,ic4))
             yfg(i,j,ic2) = .5*(yfg(i,j,ic3)+yfg(i,j,ic4))

             it1 = ((1+isen)*ic3+(1-isen)*ic4)/2
             it3 = ((1+isen)*ic4+(1-isen)*ic3)/2

             xcd(i,j,1) = xfg(i,j,it1)
             ycd(i,j,1) = yfg(i,j,it1)
             xcd(i,j,2) = xfg(i,j,ic2)
             ycd(i,j,2) = yfg(i,j,ic2)
             xcd(i,j,3) = xfg(i,j,it3)
             ycd(i,j,3) = yfg(i,j,it3)
             istat = istat + 1
          END IF                                  !NCT=1 ICLS=4
 
          IF (istat /= 1) THEN
             IF (area(i,j) > (1.-ep) .AND. istat > 1) THEN
                nfl(i,j) = 0
             ELSE
                iflt = 2
                WRITE(msg,*) 'ISTAT=',istat 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                CALL fire_error_debug(i,j,iflt,          &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
             END IF
          END IF
 
       END IF                    !NFL outer loop

    END DO x_cd
    END DO y_cd

    call wrf_debug(_FR_DBG_LEVEL_,'in fire_ln: 7')

! ----- test prints for missed grids

    DO j = jf_st+1,jf_en-1
    DO i = if_st+1,if_en-1
       IF (nfl(i,j) == 1 .AND. ncod(i,j) == 0) THEN
          iflt = 3
          CALL fire_error_debug(i,j,iflt,                &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
       END IF
    END DO
    END DO
    call wrf_debug(_FR_DBG_LEVEL_,'in fire_ln: 8')

!        ...CALCULATING INDEX LOCATIONS OF NEIGHBORS
!
!        ...This is the most critical loop of the code. If this fails 
!        the remaining logic will do weird things. This is probably 
!        the first place to check when problems occur.
 
y_in: DO j = jf_st+1,jf_en-1
x_in: DO i = if_st+1,if_en-1


       IF (nfl(i,j) == 1) THEN    !NFL=1 loop

          WRITE(msg,*) 'in fire_ln: 8.1 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          nh0 = ncod(i,j)
          nct = icn(i,j,1) + icn(i,j,2) + icn(i,j,3) + icn(i,j,4)
          icls = icl(i,j)
 
          itt = nh0
          t1 = ABS(xcd(i,j,2)) + ABS(ycd(i,j,2))          
          IF (nct == 3 .AND. icls == 2 .AND. t1 > .9) itt = 2
          tlx = SIGN(1.,xcd(i,j,itt))                         
          tly = SIGN(1.,ycd(i,j,itt))                         
          i2 = i + INT(tlx*1.5)
          j2 = j + INT(tly*1.5) 
          WRITE(msg,*) 'in fire_ln: 8.1.1 : nh0   = ',nh0 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.1.2 : nct   = ',nct 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.1.3 : icls  = ',icls 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.1.4 : itt   = ',itt 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.1.5 : i,j   = ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.1.6 : tlx   = ',tlx,tly 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.1.7 : int   = ',INT(tlx*1.5),INT(tly*1.5) 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.1.8 : i2,j2 =',i2,j2 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
 
          ihit = 0
          j2tst = j2
          i2tst = i2

          WRITE(msg,*) 'in fire_ln: 8.2 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          IF (nct == 3 .AND. icls == 1) THEN     !test code follows 
             WRITE(msg,*) 'in fire_ln: 8.2.1 : ',i,j 
             call wrf_debug(_FR_DBG_LEVEL_ ,msg)
! -----     ... looking for anomolous 3/1 situations
             ic1 = 1 - icn(i,j,1) + 2*(1-icn(i,j,2)) + 3*(1-icn(i,j,3))    &
                                  + 4*(1-icn(i,j,4))
             t1tst = ABS(xcd(i,j,1) + xcd(i,j,2))*FLOAT(iyb(i,j,ic1))      &
                   + ABS(ycd(i,j,1) + ycd(i,j,2))*FLOAT(ixb(i,j,ic1))
             IF (t1tst > 1.-ep) THEN
                j2tst = j2
                i2tst = i2
                ihit = 1
                IF (nfl(i,j2) == 0 .AND. iyb(i,j,ic1) == 0) THEN
                   tlx = SIGN(1.,xcd(i,j,2)-xcd(i,j,1))
                   i2 = i + INT(tlx*1.5)
                END IF
                IF (nfl(i2,j) == 0 .AND. ixb(i,j,ic1) == 0) THEN
                   tly = SIGN(1.,ycd(i,j,2)-ycd(i,j,1))
                   j2 = j + INT(tly*1.5)
                END IF
             END IF
          END IF                               !test code above
!
          WRITE(msg,*) 'in fire_ln: 8.3 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          x02 = xcd(i,j2,1) + SIGN(ep,xcd(i,j2,2)-xcd(i,j2,1))
          y02 = ycd(i,j2,1) + SIGN(ep,ycd(i,j2,2)-ycd(i,j2,1))
          x20 = xcd(i2,j,1) + SIGN(ep,xcd(i2,j,2)-xcd(i2,j,1))
          y20 = ycd(i2,j,1) + SIGN(ep,ycd(i2,j,2)-ycd(i2,j,1))
          x22 = xcd(i2,j2,1) + SIGN(ep,xcd(i2,j2,2)-xcd(i2,j2,1))
          y22 = ycd(i2,j2,1) + SIGN(ep,ycd(i2,j2,2)-ycd(i2,j2,1))
          r02 = (xcd(i,j,itt)-x02)**2 + (ycd(i,j,itt)-tly-y02)**2      &
               + FLOAT(1-nfl(i,j2))
          r20 = (xcd(i,j,itt)-tlx-x20)**2 + (ycd(i,j,itt)-y20)**2      &
               + FLOAT(1-nfl(i2,j))
          r22 = (xcd(i,j,itt)-tlx-x22)**2 + (ycd(i,j,itt)-tly-y22)**2  &
               + FLOAT(1-nfl(i2,j2))
          WRITE(msg,*) 'in fire_ln: 8.3.1 : ',i2,j2 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          is = i2
          js = j2
          IF (r02 < r22 .AND. r02 < r20) is = i
          IF (r20 < r22 .AND. r20 < r02) js = j
          i2 = is
          j2 = js
          WRITE(msg,*) 'in fire_ln: 8.3.2 : ',i2,j2 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
 
          itt = 1
          t1 = ABS(xcd(i,j,2)) + ABS(ycd(i,j,2))        
          IF (nct == 3 .AND. icls == 2 .AND. t1 > .9) itt = 2
          tlx = SIGN(1.,xcd(i,j,itt))                         
          tly = SIGN(1.,ycd(i,j,itt))                         
          i1 = i + INT(tlx*1.5)
          j1 = j + INT(tly*1.5) 
 
          j1tst = j1
          i1tst = i1
          WRITE(msg,*) 'in fire_ln: 8.4 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
 
          IF (nct == 3 .AND. icls == 1) THEN     !test code follows 
             WRITE(msg,*) 'in fire_ln: 8.4.1 : ',i,j 
             call wrf_debug(_FR_DBG_LEVEL_ ,msg)
! -----      ... looking for anomolous 3/1 situations
             ic1 = 1 - icn(i,j,1) + 2*(1-icn(i,j,2)) + 3*(1-icn(i,j,3))  &
                + 4*(1-icn(i,j,4))
             t1tst = ABS(xcd(i,j,1) + xcd(i,j,2))*FLOAT(iyb(i,j,ic1))  &
                   + ABS(ycd(i,j,1) + ycd(i,j,2))*FLOAT(ixb(i,j,ic1))
             IF (t1tst > 1.-ep) THEN
                j1tst = j1
                i1tst = i1
                ihit = ihit + 2
                if (nfl(i,j1) == 0 .AND. iyb(i,j,ic1) == 0) THEN
                   tlx = SIGN(1.,xcd(i,j,1)-xcd(i,j,2))
                   i1 = i + INT(tlx*1.5)
                END IF
                IF (nfl(i1,j) == 0 .AND. ixb(i,j,ic1) == 0) THEN
                   tly = SIGN(1.,ycd(i,j,1)-ycd(i,j,2))
                   j1 = j + INT(tly*1.5)
                END IF
             END IF
          END IF                               !test code above
!
          WRITE(msg,*) 'in fire_ln: 8.5 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.1 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          nh11 = nfl(i1,j1)*ncod(i1,j1) + 1 - nfl(i1,j1)        
          nh01 = nfl(i ,j1)*ncod(i ,j1) + 1 - nfl(i ,j1)        
          nh10 = nfl(i1, j)*ncod(i1, j) + 1 - nfl(i1, j)        
          nhtmp = nh01 - nfl(i,j1)
          x01 = xcd(i,j1,nh01) + SIGN(ep,xcd(i,j1,nhtmp)-xcd(i,j1,nh01))
          y01 = ycd(i,j1,nh01) + SIGN(ep,ycd(i,j1,nhtmp)-ycd(i,j1,nh01))
          WRITE(msg,*) 'in fire_ln: 8.5.2 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          nhtmp = nh10 - nfl(i1,j)
          x10 = xcd(i1,j,nh10) + SIGN(ep,xcd(i1,j,nhtmp)-xcd(i1,j,nh10))
          y10 = ycd(i1,j,nh10) + SIGN(ep,ycd(i1,j,nhtmp)-ycd(i1,j,nh10))
          nhtmp = nh11 - nfl(i1,j1)
          x11 = xcd(i1,j1,nh11) + SIGN(ep,xcd(i1,j1,nhtmp)-xcd(i1,j1,nh11))
          y11 = ycd(i1,j1,nh11) + SIGN(ep,ycd(i1,j1,nhtmp)-ycd(i1,j1,nh11))
          r01 = (xcd(i,j,itt)-x01)**2 + (ycd(i,j,itt)-tly-y01)**2      &
                + (1.-FLOAT(nfl(i,j1)))
          r10 = (xcd(i,j,itt)-tlx-x10)**2 + (ycd(i,j,itt)-y10)**2      &
                + (1.-FLOAT(nfl(i1,j)))
          r11 = (xcd(i,j,itt)-tlx-x11)**2 + (ycd(i,j,itt)-tly-y11)**2  &
               + (1.-FLOAT(nfl(i1,j1)))
          WRITE(msg,*) 'in fire_ln: 8.5.3 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          is = i1
          js = j1
          IF (r01 < r11 .AND. r01 < r10) is = i
          IF (r10 < r11 .AND. r10 < r01) js = j
          i1 = is
          j1 = js
          WRITE(msg,*) 'in fire_ln: 8.5.4 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
! -----   ... end of evaluation
          in1(i,j,1) = i1
          in1(i,j,2) = j1
          in2(i,j,1) = i2
          in2(i,j,2) = j2
          nh1 = ncod(i1,j1)
          nh2 = ncod(i2,j2)
          nht = nh0 + nh1 + nh2
 
          WRITE(msg,*) 'in fire_ln: 8.5.5 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.5 : ncod(',i1,j1,') = ',ncod(i1,j1),nfl(i1,j1) 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.5 : ncod(',i2,j2,') = ',ncod(i2,j2),nfl(i2,j2) 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.5 : xcd(',i,j,1,') = ',xcd(i,j,1) 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.5 : xcd(',i2,j2,nh2,') = ',xcd(i2,j2,nh2) 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.5 : i2-i  = ',i2-i 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.5 : ycd(',i,j,1,') = ',ycd(i,j,1) 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.5 : ycd(',i2,j2,nh2,') = ',ycd(i2,j2,nh2) 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.5 : j2-j  = ',j2-j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)

          WRITE(msg,*) 'in fire_ln: 8.5.5 : xcd(',i,j,nh0,') = ',xcd(i,j,nh0) 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.5 : xcd(',i1,j1,1,') = ',xcd(i1,j1,1) 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.5 : i1-i  = ',i1-i 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.5 : ycd(',i,j,nh0,') = ',ycd(i,j,nh0) 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.5 : ycd(',i1,j1,1,') = ',ycd(i1,j1,1) 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          WRITE(msg,*) 'in fire_ln: 8.5.5 : j1-j  = ',j1-j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          r2sq = (xcd(i,j,1)-xcd(i2,j2,nh2)-FLOAT(i2-i))**2 +    &
                 (ycd(i,j,1)-ycd(i2,j2,nh2)-FLOAT(j2-j))**2
          r1sq = (xcd(i,j,nh0)-xcd(i1,j1,1)-FLOAT(i1-i))**2 +    &
                 (ycd(i,j,nh0)-ycd(i1,j1,1)-FLOAT(j1-j))**2

!         IF (nh0 == 0) then
!            r2sq = (xcd(i,j,1)-xcd(i2,j2,nh2)-FLOAT(i2-i))**2 +    &
!                   (ycd(i,j,1)-ycd(i2,j2,nh2)-FLOAT(j2-j))**2
!            IF (r2sq < ep_sq) THEN
!               iflt = 184
!               CALL fire_error_debug(i,j,iflt,                        &
!                                     time,in1,in2,tign_g,             &
!                                     nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
!                                     ixb,iyb,xfg,yfg,                 &
!                                     ifms,ifme, kfms,kfme, jfms,jfme, &
!                                     if_st,if_en,jf_st,jf_en)
!            END IF
!         ELSE IF (nh2 == 0) THEN
!            r1sq = (xcd(i,j,nh0)-xcd(i1,j1,1)-FLOAT(i1-i))**2 +    &
!                   (ycd(i,j,nh0)-ycd(i1,j1,1)-FLOAT(j1-j))**2
!            IF (r1sq < ep_sq) THEN
!               iflt = 185
!               CALL fire_error_debug(i,j,iflt,                        &
!                                     time,in1,in2,tign_g,             &
!                                     nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
!                                     ixb,iyb,xfg,yfg,                 &
!                                     ifms,ifme, kfms,kfme, jfms,jfme, &
!                                     if_st,if_en,jf_st,jf_en)
!            END IF
!         ELSE
!            r2sq = (xcd(i,j,1)-xcd(i2,j2,nh2)-FLOAT(i2-i))**2 +    &
!                   (ycd(i,j,1)-ycd(i2,j2,nh2)-FLOAT(j2-j))**2
!            r1sq = (xcd(i,j,nh0)-xcd(i1,j1,1)-FLOAT(i1-i))**2 +    &
!                   (ycd(i,j,nh0)-ycd(i1,j1,1)-FLOAT(j1-j))**2
!            IF (r1sq < ep_sq .OR. r2sq < ep_sq) THEN
!               WRITE(msg,*) 'DEBUG R1SQ R2SQ=',r1sq,r2sq 
!               call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!               iflt = 84
!               CALL fire_error_debug(i,j,iflt,                        &
!                                        time,in1,in2,tign_g,          &
!                                     ixb,iyb,xfg,yfg,                 &
!                                     ifms,ifme, kfms,kfme, jfms,jfme, &
!                                     if_st,if_en,jf_st,jf_en)
!
!             END IF
!          END IF

          WRITE(msg,*) 'in fire_ln: 8.5.6 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)

!         IF (r1sq < ep_sq .OR. r2sq < ep_sq) THEN
!            WRITE(msg,*) 'DEBUG R1SQ R2SQ=',r1sq,r2sq 
!            call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!            iflt = 84
!            CALL fire_error_debug(i,j,iflt,                        &
!                                  time,in1,in2,tign_g,             &
!                                  nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
!                                  ixb,iyb,xfg,yfg,                 &
!                                  ifms,ifme, kfms,kfme, jfms,jfme, &
!                                  if_st,if_en,jf_st,jf_en)

!         END IF
 
          WRITE(msg,*) 'in fire_ln: 8.6 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          IF (i1 < if_st .OR. i1 > if_en .OR. j1 < jf_st .OR. j1 > jf_en) THEN
             iflt = 85
             CALL fire_error_debug(i,j,iflt,             &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
          END IF
          IF (i2 < if_st .OR. i2 > if_en .OR. j2 < jf_st .OR. j2 > jf_en) THEN
             iflt = 87
             CALL fire_error_debug(i,j,iflt,             &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
          END IF
  
          WRITE(msg,*) 'in fire_ln: 8.7 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          ilm1 = IABS(in1(i,j,1)-i)
          ilm2 = IABS(in2(i,j,1)-i)
          ilm3 = IABS(in1(i,j,2)-j)
          ilm4 = IABS(in2(i,j,2)-j)

          WRITE(msg,*) 'in fire_ln: 8.8 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          IF (ilm1 > 1 .OR. ilm2 > 1 .OR. ilm3 > 1 .OR. ilm4 > 1) THEN
             iflt = 83
             CALL fire_error_debug(i,j,iflt,             &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
          END IF

          IF (i1 == i .AND. j1 == j) THEN
             iflt = 4
             CALL fire_error_debug(i,j,iflt,             &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
          END IF

          IF (i2 == i .AND. j2 == j) THEN
             iflt = 5
             CALL fire_error_debug(i,j,iflt,             &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
          END IF

          IF (i2 == i1 .AND. j2 == j1) THEN
! -----      ... fireline brushes corner of 3/1.  Odd normal vector pts inwards.
             iflt = 6
             WRITE(msg,*) 'IC1 T1TST=',ic1,t1tst 
             call wrf_debug(_FR_DBG_LEVEL_ ,msg)
             WRITE(msg,*) 'IHIT=',ihit 
             call wrf_debug(_FR_DBG_LEVEL_ ,msg)
             WRITE(msg,*) 'I1TST J1TST=',i1tst,j1tst 
             call wrf_debug(_FR_DBG_LEVEL_ ,msg)
             WRITE(msg,*) 'I2TST J2TST=',i2tst,j2tst 
             call wrf_debug(_FR_DBG_LEVEL_ ,msg)
             WRITE (msg,*) 'i,i2,i1=',i,i2,i1,' j,j2,j1=',j,j2,j1
             CALL wrf_message( msg )
             CALL fire_error_debug(i,j,iflt,             &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
          END IF
 
          WRITE(msg,*) 'in fire_ln: 8.9 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
          itest = 0
          IF (itest == 0) THEN

             IF (i1 /= in1(i,j,1) .OR. j1 /= in1(i,j,2) .OR. i2 /= in2(i,j,1)  &
                       .OR. j2 /= in2(i,j,2)) THEN
                iflt = 80
                CALL fire_error_debug(i,j,iflt,          &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
             END IF

             IF (ABS(xcd(i,j,1)) > .5 .OR. ABS(ycd(i,j,1)) > .5) THEN
                iflt = 81
                CALL fire_error_debug(i,j,iflt,          &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
             END IF

             IF (ABS(xcd(i,j,nh0)) > .5 .OR. ABS(ycd(i,j,nh0)) > .5) THEN
                iflt = 82
                CALL fire_error_debug(i,j,iflt,          &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
             END IF

          END IF
          WRITE(msg,*) 'in fire_ln: 8.10 : ',i,j 
          call wrf_debug(_FR_DBG_LEVEL_ ,msg)
       END IF                     !NFL=1 LOOP
!
    END DO x_in
    END DO y_in

    WRITE(msg,*) 'in fire_ln: 9' 
    call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!
    radmax = -1.e5
    radmin =  1.e5
    radavg = 0.0
    radsum = 0.0
    radtst = 10000.

!     ...This is the second most critical loop of the code. If this 
!     fails the remaining logic may fail. This is probably the second
!     place to check when problems occur.
 
y_cn: DO j = jf_st+1,jf_en-1
x_cn: DO i = if_st+1,if_en-1

       IF (nfl(i,j) == 1) THEN    !NFL=1 LOOP

          nh0 = ncod(i,j)
          i1 = in1(i,j,1)
          j1 = in1(i,j,2)
          i2 = in2(i,j,1)
          j2 = in2(i,j,2)
          nh1 = ncod(i1,j1)
          nh2 = ncod(i2,j2)
  
! -----  ...calculate xcn and ycn for points 1,nh0 at grid i,j
 
          x1 = xcd(i1,j1,  1) + FLOAT(i1-i)
          y1 = ycd(i1,j1,  1) + FLOAT(j1-j)
          x2 = xcd(i ,j ,  1)
          y2 = ycd(i ,j ,  1)
          x3 = xcd(i ,j ,nh0)
          y3 = ycd(i ,j ,nh0)
          iloc = 0

          IF (icl(i,j) == 4 .AND. nc(i,j) == 1) THEN
             IF (icl(i1,j1) == 4 .AND. nc(i1,j1) == 1) THEN
                i3 = in1(i1,j1,1)
                j3 = in1(i1,j1,2)
!               IF (nfl(i3,j3) == 1) THEN
                IF (nfl(i3,j3) == 1 .AND. icl(i3,j3) /= 4) THEN
                   iloc = 1
                   x1 = xcd(i3,j3,1) + FLOAT(i3-i)
                   y1 = ycd(i3,j3,1) + FLOAT(j3-j)
                   x2 = xcd(i ,j ,  1)
                   y2 = ycd(i ,j ,  1)
                   x3 = xcd(i2,j2,nh2) + FLOAT(i2-i)
                   y3 = ycd(i2,j2,nh2) + FLOAT(j2-j)
                END IF
             END IF
          END IF

!        ... test code follows
          IF (icl(i,j) == 4 .AND. nc(i,j) == 1 .AND. iloc == 0) THEN
             x1 = xcd(i1,j1,  1) + FLOAT(i1-i)
             y1 = ycd(i1,j1,  1) + FLOAT(j1-j)
             x2 = xcd(i1,j1,nh1) + FLOAT(i1-i)
             y2 = ycd(i1,j1,nh1) + FLOAT(j1-j)
             x3 = xcd(i2,j2,  1) + FLOAT(i2-i)
             y3 = ycd(i2,j2,  1) + FLOAT(j2-j)
          END IF
!        ... test code above
 
          xcn(i,j,1) = xcd(i,j,1)
          ycn(i,j,1) = ycd(i,j,1)
          det = (x1-x2)*(y1-y3) - (x1-x3)*(y1-y2)
          aa = .5*(x1*x1 - x2*x2 + y1*y1 - y2*y2)
          bb = .5*(x1*x1 - x3*x3 + y1*y1 - y3*y3)
          x_0 = (aa*(y1-y3)-bb*(y1-y2)) / (det+SIGN(ep,det))
          y_0 = (bb*(x1-x2)-aa*(x1-x3)) / (det+SIGN(ep,det))
          rad = SQRT((x2-x_0)**2 + (y2-y_0)**2)
          IF (rad > 1. .AND. rad < 1000.) THEN             !test
             vtsgn = (y3-y1)*(x2-x_0) - (x3-x1)*(y2-y_0)
             it1 = INT((vtsgn+ABS(vtsgn)) / (ABS(vtsgn)+ep_sq)+.5) - 1
             t1 = FLOAT(it1)
             xcn(i,j,1) = xcd(i,j,1) + t1*(x2-x_0)/(rad+ep)
             ycn(i,j,1) = ycd(i,j,1) + t1*(y2-y_0)/(rad+ep)
          ELSE
             rad = SQRT((y3-y1)**2 + (x3-x1)**2)
             IF (ABS(rad) < ep) THEN
                WRITE(msg,*)  'HEY1 START' 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY1 RAD,ILOC',rad,iloc 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY1 I,J,NH0',i,j,nh0 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY1 I1,J1',i1,j1,nh1 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY1 R01,R10,R11',r01,r10,r11 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY1 X1,X2,X3',x1,x2,x3 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY1 Y1,Y2,Y3',y1,y2,y3 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY1 XCD(I)=',(xcd(i,j,it),it=1,nh0) 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY1 YCD(I)=',(ycd(i,j,it),it=1,nh0) 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY1 XCD(I1)=',(xcd(i1,j1,it),it=1,nh1) 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY1 YCD(I1)=',(ycd(i1,j1,it),it=1,nh1) 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                iflt = 111
                CALL fire_error_debug(i,j,iflt,          &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
             END IF
             xcn(i,j,1) = xcd(i,j,1) + (y3-y1)/(rad+ep)
             ycn(i,j,1) = ycd(i,j,1) - (x3-x1)/(rad+ep)
          END IF
 
          IF (rad < .1 .AND. rad < radtst) THEN
             radhld = rad
             radtst = rad
             ihld = i
             jhld = j
          END IF              
 
          radmax = MAX(radmax,rad)
          radmin = MIN(radmin,rad)
          radavg = radavg + rad
          radsum = radsum + 1.
     
          x1 = xcd(i ,j ,  1)
          y1 = ycd(i ,j ,  1)
          x2 = xcd(i ,j ,nh0)
          y2 = ycd(i ,j ,nh0)
          x3 = xcd(i2,j2,nh2) + FLOAT(i2-i)
          y3 = ycd(i2,j2,nh2) + FLOAT(j2-j)
          iloc = 0

          IF (icl(i,j) == 4 .AND. nc(i,j) == 1) THEN
             IF (icl(i2,j2) == 4 .AND. nc(i2,j2) == 1) THEN
                i3 = in2(i2,j2,1)
                j3 = in2(i2,j2,2)
                nh3 = ncod(i3,j3)
!               IF (nfl(i3,j3) == 1) THEN
                IF (nfl(i3,j3) == 1 .AND. icl(i3,j3) /= 4) THEN
                   iloc = 1
                   x1 = xcd(i1,j1,1) + FLOAT(i1-i)
                   y1 = ycd(i1,j1,1) + FLOAT(j1-j)
                   x2 = xcd(i ,j ,nh0)
                   y2 = ycd(i ,j ,nh0)
                   x3 = xcd(i3,j3,nh3) + FLOAT(i3-i)
                   y3 = ycd(i3,j3,nh3) + FLOAT(j3-j)
                END IF
             END IF
          END IF

!        ... test code follows
          IF (icl(i,j) == 4 .AND. nc(i,j) == 1 .AND. iloc == 0) THEN
             x1 = xcd(i1,j1,nh1) + FLOAT(i1-i)
             y1 = ycd(i1,j1,nh1) + FLOAT(j1-j)
             x2 = xcd(i2,j2,  1) + FLOAT(i2-i)
             y2 = ycd(i2,j2,  1) + FLOAT(j2-j)
             x3 = xcd(i2,j2,nh2) + FLOAT(i2-i)
             y3 = ycd(i2,j2,nh2) + FLOAT(j2-j)
          END IF
!        ... test code above

          xcn(i,j,nh0) = xcd(i,j,nh0)
          ycn(i,j,nh0) = ycd(i,j,nh0)
          det = (x1-x2)*(y1-y3) - (x1-x3)*(y1-y2)
          aa = .5*(x1*x1 - x2*x2 + y1*y1 - y2*y2)
          bb = .5*(x1*x1 - x3*x3 + y1*y1 - y3*y3)
          x_0 = (aa*(y1-y3) - bb*(y1-y2)) / (det + SIGN(ep,det))
          y_0 = (bb*(x1-x2) - aa*(x1-x3)) / (det + SIGN(ep,det))
          rad = SQRT((x2-x_0)**2 + (y2-y_0)**2)

          IF (rad > 1. .AND. rad < 1000.) THEN             !test
             vtsgn = (y3-y1)*(x2-x_0) - (x3-x1)*(y2-y_0)
             it1 = INT((vtsgn+ABS(vtsgn)) / (ABS(vtsgn)+ep_sq)+.5) - 1
             t1 = FLOAT(it1)
             xcn(i,j,nh0) = xcd(i,j,nh0) + t1*(x2-x_0)/(rad+ep)
             ycn(i,j,nh0) = ycd(i,j,nh0) + t1*(y2-y_0)/(rad+ep)
          ELSE
             rad = SQRT((y3-y1)**2 + (x3-x1)**2)
             IF (abs(rad) < ep) THEN
                WRITE(msg,*)  'HEY2 START' 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY2 RAD,ILOC',rad,iloc 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY2 I,J,NH0',i,j,nh0 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY2 I2,J2,NH2',i2,j2,nh2 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY2 R02,R20,R22',r02,r20,r22 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY2 X1,X2,X3',x1,x2,x3 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY2 Y1,Y2,Y3',y1,y2,y3 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY2 XCD(I)=',(xcd(i,j,it),it=1,nh0) 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY2 YCD(I)=',(ycd(i,j,it),it=1,nh0) 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY2 XCD(I2)=',(xcd(i2,j2,it),it=1,nh2) 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'HEY2 YCD(I2)=',(ycd(i2,j2,it),it=1,nh2) 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                iflt = 112
                CALL fire_error_debug(i,j,iflt,          &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
             END IF
             xcn(i,j,nh0) = xcd(i,j,nh0) + (y3-y1)/(rad+ep)
             ycn(i,j,nh0) = ycd(i,j,nh0) - (x3-x1)/(rad+ep)
          END IF
          IF (rad < .1 .AND. rad < radtst) THEN
             radhld = rad
             radtst = rad
             ihld = i
             jhld = j
          END IF                         
 
          IF (nh0 == 3) THEN
             x1 = xcd(i,j,1)
             y1 = ycd(i,j,1)
             x3 = xcd(i,j,3)
             y3 = ycd(i,j,3)
             rad = SQRT((y3-y1)**2 + (x3-x1)**2)
             IF (ABS(rad) < ep) THEN
                 WRITE(msg,*)  'HEY3 I,J,RAD,Y1,Y2,Y3,X1,X2,X3', &
                 i,j,rad,y1,y2,y3,x1,x2,x3
                iflt = 113
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                CALL fire_error_debug(i,j,iflt,          &
                        time,in1,in2,tign_g,             &
                        nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                        ixb,iyb,xfg,yfg,                 &
                        ifms,ifme, kfms,kfme, jfms,jfme, &
                        if_st,if_en,jf_st,jf_en)
             END IF
             xcn(i,j,2) = xcd(i,j,2) + (y3-y1)/(rad+ep)
             ycn(i,j,2) = ycd(i,j,2) - (x3-x1)/(rad+ep)
          END IF
       END IF                     !NFL=1 LOOP

    END DO x_cn
    END DO y_cn

    call wrf_debug(_FR_DBG_LEVEL_,'in fire_ln: 10')
    IF (ABS(radsum) > ep_sq) THEN
      radavg = radavg/radsum
      WRITE(msg,*) 'RADMAX MIN AVG SUM',radmax,radmin,radavg,radsum 
      call wrf_debug(_FR_DBG_LEVEL_ ,msg)
    ELSE
      WRITE(msg,*) 'RADMAX MIN SUM, CANT CALC RADAVG',radmax,radmin,radsum 
      call wrf_debug(_FR_DBG_LEVEL_ ,msg)
    END IF
    call wrf_debug(_FR_DBG_LEVEL_,'in fire_ln: 11')

    RETURN

END SUBROUTINE fire_ln

! =========================================================================

SUBROUTINE fire_tr(dt,ibeh,nfuel_cat,ncod,nfl,zf,zsf,zs,        &  ! incoming
                   sfcu,sfcv,xcd,ycd,bbb,phiwc,betafl,r_0,      &
                   ids,ide, kds,kde, jds,jde,                   &
                   ims,ime, kms,kme, jms,jme,                   &
                   its,ite, kts,kte, jts,jte,                   &
                   ifms,ifme, kfms,kfme, jfms,jfme,             &
                   if_st,if_en,jf_st,jf_en,                     &
                   nfrx,nfry,                                   &
                   xcn,ycn,                                     &  ! inout
                   sprdx,sprdy)                                    ! outgoing

! --------------------------------------------------------------------
!   this routine advects fire line coordinates after Fire_ln call           
!      all xcd ycd xcn and ycn points are calculated and used to 
!      calculate local velocities and spread rates
! --------------------------------------------------------------------

   USE module_fr_cawfe_fuel

   IMPLICIT NONE

! ------ incoming variables

   INTEGER, INTENT(in) :: ids,ide, kds,kde, jds,jde
   INTEGER, INTENT(in) :: ims,ime, kms,kme, jms,jme
   INTEGER, INTENT(in) :: its,ite, kts,kte, jts,jte
   INTEGER, INTENT(in) :: ifms,ifme, kfms,kfme, jfms,jfme
   INTEGER, INTENT(in) :: if_st,if_en, jf_st,jf_en
   INTEGER, INTENT(in) :: nfrx,nfry

   INTEGER, INTENT(in) :: ibeh

   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme )   :: nfuel_cat
   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme )   :: ncod,nfl

   REAL, INTENT(in)  :: dt

   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme,6 ) :: sfcu,sfcv
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xcd,ycd
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme )   :: zf,zsf
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme )   :: zs
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme )   :: bbb,phiwc,betafl,r_0

! ------ inout variables

   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xcn,ycn

! ------ outgoing variables

   REAL, INTENT(out), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: sprdx,sprdy

! ------ local variables

   INTEGER :: i,j
   INTEGER :: it,itm
   INTEGER :: ks,ksp
   INTEGER :: ib,jb

   REAL :: tirx,tiry
   REAL :: tspmax,tspmin
   REAL :: epx,epy,epz
   REAL :: t1
   REAL :: dlx,dly,dl
   REAL :: x,y
   REAL :: tib,tjb
   REAL :: uu,vv
   REAL :: zs1,zs2
   REAL :: tanphi,speed
   REAL :: ss
   REAL :: fuel_hgt

   REAL, PARAMETER :: dist = 2.		! distance behind fire line in the
					! direction of the normal vector that
					! winds are taken to advect fire.
					! terrain slope taken to be the
					! difference between the elevation at
					! the fire line and the elevation twice
					! this distance from the fire line in
					! the direction of the normal vector.

   CHARACTER(LEN=80) :: msg

!    when deriving fireline coordinates we always keep the fire to our left
 
    tirx = 1. / FLOAT(nfrx)
    tiry = 1. / FLOAT(nfry)
 
!     ... SFCU and SFCV are positioned such that vertical index=1 means 
!        they are at the surface (2) means one grid point above the surface.
 
    tspmax = 0.0
    tspmin = 1000.0

    DO j = jf_st,jf_en
    DO i = if_st,if_en

! --- get integer height of fuel

       fuel_hgt = 1. + fueldepthm(nfuel_cat(i,j)) / (zf(i,j)-zsf(i,j))

       ks = INT( fuel_hgt )
       ksp = ks+1                        ! test!
       epz = fuel_hgt - FLOAT(ks)        ! test!

! --- zero out some debug

       DO it = 1,4
          sprdx(i,j,it) = 0.0
          sprdy(i,j,it) = 0.0 
       END DO

       itm = ncod(i,j)
 
       IF (nfl(i,j).eq.1) THEN                      !NFL bypass

          DO it = 1,itm
 
       ! -- the Don Latham change follows - jury is still out

             dlx = (xcn(i,j,it)-xcd(i,j,it)) * dxf
             dly = (ycn(i,j,it)-ycd(i,j,it)) * dyf

       ! --- find winds at the specified distance behind the fire line.
       !     note that ib, jb are atmospheric grid coords. (where 
       !     zs is defined), not fuel cell grid coords.

       !jbeezley : added to fix the case with dlx==dly==0 :
       !           perhaps the physics is wrong?
             IF(dlx.ne.0.or.dly.ne.0) THEN
               t1 = dist / SQRT( dlx**2 + dly**2 )                      
               x = xcd(i,j,it) - (xcn(i,j,it)-xcd(i,j,it))*t1
               y = ycd(i,j,it) - (ycn(i,j,it)-ycd(i,j,it))*t1
             ELSE
               x = 0
               y = 0
             ENDIF

             tib = 1. + (FLOAT(i)-1.5+x)*tirx
             tjb = 1. + (FLOAT(j)-1.5+y)*tiry
             ib = INT(tib)
             jb = INT(tjb)
             epx = tib - FLOAT(ib)
             epy = tjb - FLOAT(jb)

             uu = (1.-epz)*(                                    &
                        (1.-epy)*((1.-epx)*sfcu(ib  ,jb  ,ks)   &
                                    + epx *sfcu(ib+1,jb  ,ks))  &
                           + epy*((1.-epx)*sfcu(ib  ,jb+1,ks)   &
                                    + epx *sfcu(ib+1,jb+1,ks))) &
                    + epz *(                                    &
                        (1.-epy)*((1.-epx)*sfcu(ib  ,jb  ,ksp)  &
                                    + epx *sfcu(ib+1,jb  ,ksp)) &
                           + epy*((1.-epx)*sfcu(ib  ,jb+1,ksp)  &
                                    + epx *sfcu(ib+1,jb+1,ksp)))

             vv = (1.-epz)*(                                    &
                        (1.-epy)*((1.-epx)*sfcv(ib  ,jb  ,ks)   &
                                    + epx *sfcv(ib+1,jb  ,ks))  &
                           + epy*((1.-epx)*sfcv(ib  ,jb+1,ks)   &
                                    + epx *sfcv(ib+1,jb+1,ks))) &
                    + epz *(                                    &
                        (1.-epy)*((1.-epx)*sfcv(ib  ,jb  ,ksp)  &
                                    + epx *sfcv(ib+1,jb  ,ksp)) &
                           + epy*((1.-epx)*sfcv(ib  ,jb+1,ksp)  &
                                    + epx *sfcv(ib+1,jb+1,ksp)))
 
       ! --- find elevation at the fire line for this particle.

             t1 = 0.0
             x = xcd(i,j,it) - (xcn(i,j,it)-xcd(i,j,it))*t1
             y = ycd(i,j,it) - (ycn(i,j,it)-ycd(i,j,it))*t1
             tib = 1.5 + (FLOAT(i)-1.5+x)*tirx
             tjb = 1.5 + (FLOAT(j)-1.5+y)*tiry
             ib = INT(tib)
             jb = INT(tjb)
             epx = tib - FLOAT(ib)
             epy = tjb - FLOAT(jb)

             zs1 = (1.-epy)*((1.-epx)*zs(ib,jb  ) + epx*zs(ib+1,jb))  &
                      + epy*((1.-epx)*zs(ib,jb+1) + epx*zs(ib+1,jb+1))

       ! --- find elevation at twice the specified distance behind 
       !     the fire line in the direction of the normal vector

             IF(dlx.ne.0.or.dly.ne.0) THEN
               t1 = dist / SQRT( dlx**2 + dly**2 )
               x = xcd(i,j,it) - (xcn(i,j,it)-xcd(i,j,it))*t1
               y = ycd(i,j,it) - (ycn(i,j,it)-ycd(i,j,it))*t1
             ELSE
               x = 0
               y = 0
             ENDIF

             tib = 1.5 + (FLOAT(i)-1.5+x)*tirx
             tjb = 1.5 + (FLOAT(j)-1.5+y)*tiry
             ib = INT(tib)
             jb = INT(tjb)
             epx = tib - FLOAT(ib)
             epy = tjb - FLOAT(jb)

             zs2 = (1.-epy)*((1.-epx)*zs(ib,jb  ) + epx*zs(ib+1,jb))  &
                      + epy*((1.-epx)*zs(ib,jb+1) + epx*zs(ib+1,jb+1))

       ! --- calculate  tangent of terrain slope in direction of spread.

             tanphi = (zs1-zs2) / (2.*dist)
             WRITE(msg,*) 'debug ZS1 ZS2 TANPHI=',zs1,zs2,tanphi 
             call wrf_debug(_FR_DBG_LEVEL_ ,msg)
 
       ! --- calculate wind speed in direction of spread.

             dlx = (xcn(i,j,it)-xcd(i,j,it))
             dly = (ycn(i,j,it)-ycd(i,j,it))
             dlx = dlx + SIGN(ep,dlx)
             dly = dly + SIGN(ep,dly)
             dl = SQRT(dlx**2 + dly**2)

             IF(dlx.ne.0.or.dly.ne.0) THEN

               t1 = 1./dl
               speed = t1*(uu*dlx + vv*dly)

             ELSE
               
               speed = 0

             ENDIF

!            fuelloadm = (1.-bmst) * fgi(nfuel_cat(i,j)) ! fueload w/out moisture

       ! --- calculate fire's rate of spread

             CALL fire_ros( i,j,                             &    ! send
                            ifms,ifme, kfms,kfme, jfms,jfme, &
                            speed,tanphi,ibeh,nfuel_cat,     & 
                            bbb,phiwc,betafl,r_0,            &
                            ss)                                  ! recv

       ! --- get new non-dimensional distance according to spread rate
 
             dlx = t1 * dlx * ss * dt / dxf
             dly = t1 * dly * ss * dt / dyf

       ! --- some debug

             tspmax = MAX(ss,tspmax)
             tspmin = MIN(ss,tspmin)
             sprdx(i,j,it) = dxf * dlx / dt
             sprdy(i,j,it) = dyf * dly / dt

       ! --- update normal vector's locations

             xcn(i,j,it) = xcd(i,j,it) + dlx
             ycn(i,j,it) = ycd(i,j,it) + dly

          END DO
       END IF                                  !NFL bypass
    END DO
    END DO
 
    WRITE(msg,21) tspmax, tspmin
21  FORMAT(1x,'MAX/MIN SPREAD RATE (m/s)=',2(1x,f10.5))
    CALL wrf_message( msg )
 
    RETURN

END SUBROUTINE fire_tr

! =========================================================================

SUBROUTINE fire_ds(ixb,iyb,icn,nfl,ncod,in1,in2,      & ! incoming
                   time,xcn,ycn,xcd,ycd,              &
                   ifms,ifme, kfms,kfme, jfms,jfme,   &
                   if_st,if_en,jf_st,jf_en,           &
!                   nfrx,nfry,                         &
                   xfg,yfg,tign_g)                      ! inout

! ---------------------------------------------------------------------
! This routine assigns quadrilateral grid positions using the
! new XCN and YCN positions. New cell ignitions are also treated.
!
! First we calculate XFG,YFG using linear extrapolation. This
! can result in different values at the same equivalent point 
! for neighboring grids. (Curvature effect). This will be corrected
! in last loop of this routine.
! ---------------------------------------------------------------------

   IMPLICIT NONE

! ----- incoming variables

   INTEGER, INTENT(in) :: ifms,ifme, kfms,kfme, jfms,jfme
   INTEGER, INTENT(in) :: if_st,if_en, jf_st,jf_en
!   INTEGER, INTENT(in) :: nfrx,nfry

   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: ixb,iyb,icn
   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme )   :: nfl,ncod
   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,2 ) :: in1,in2

   REAL, INTENT(in) :: time

   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xcn,ycn
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xcd,ycd

! ----- inout variables

   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xfg,yfg
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: tign_g

! ----- local variables

   INTEGER :: i,j
   INTEGER :: nct,icls
   INTEGER :: i1,i2
   INTEGER :: j1,j2
   INTEGER :: nh0,nh1,nh2,nht
   INTEGER :: ic1,ic2,ic3,ic4
   INTEGER :: iod
   INTEGER :: is,js
   INTEGER :: isen
   INTEGER :: iia3,iib3,iic3,iid3,iit
   INTEGER :: jja4,jjb4,jjc4,jjd4,jjt
   INTEGER :: iflt

   INTEGER, DIMENSION( ifms:ifme,jfms:jfme )   :: nc,icl

   REAL :: dlx,dly
   REAL :: dxx,dyy
   REAL :: x1,x2,x3,x4,x5,x6,x7
   REAL :: y1,y2,y3,y4,y5,y6,y7
   REAL :: xfg_a3,xfg_b3,xfg_c3,xfg_d3
   REAL :: yfg_a4,yfg_b4,yfg_c4,yfg_d4
   REAL :: xfg_ic3,yfg_ic4
   REAL :: tia3,tib3,tic3,tid3
   REAL :: tja4,tjb4,tjc4,tjd4

   CHARACTER(LEN=256) :: msg
!  when deriving fireline coordinates we always keep the fire to our left
 
    DO j = jf_st,jf_en
    DO i = if_st,if_en
       nc(i,j)  = icn(i,j,1) + icn(i,j,2) + icn(i,j,3) + icn(i,j,4)
       icl(i,j) = ixb(i,j,1) + ixb(i,j,2) + ixb(i,j,3) + ixb(i,j,4) +  &
                  iyb(i,j,1) + iyb(i,j,2) + iyb(i,j,3) + iyb(i,j,4)
    END DO
    END DO
!
yl: DO j = jf_st+1,jf_en-1
xl: DO i = if_st+1,if_en-1

       IF (nfl(i,j) == 1) THEN   !NFL outer loop

          nct = nc(i,j)
          icls = icl(i,j)
          nh0 = ncod(i,j)
          i1 = in1(i,j,1)
          j1 = in1(i,j,2)
          i2 = in2(i,j,1)
          j2 = in2(i,j,2)
          nh1 = ncod(i1,j1)
          nh2 = ncod(i2,j2)
          nht = nh0 + nh1 + nh2

          dly = (xcn(i,j,2)-xcn(i,j,1))/                                 &
              (ycn(i,j,2)-ycn(i,j,1)+SIGN(ep,ycn(i,j,2)-ycn(i,j,1)))
          dlx = (ycn(i,j,2)-ycn(i,j,1))/                                 &
              (xcn(i,j,2)-xcn(i,j,1)+SIGN(ep,xcn(i,j,2)-xcn(i,j,1)))

          IF (nct == 3 .AND. icls == 1) THEN    !NCT=3 and ICLS=1 NCOD=2

             ic1 = 10 - icn(i,j,1) - 2*icn(i,j,2) - 3*icn(i,j,3) - 4*icn(i,j,4)

             IF (ic1 == 1) THEN             
                IF (ixb(i,j,ic1) == 1) THEN
                   dxx = xcn(i,j,1) + (ylm(ic1)-ycn(i,j,1))*dly - xfg(i,j,ic1)
                   dxx = MIN(0.0,dxx)
                   xfg(i,j,ic1) = xfg(i,j,ic1) + dxx
                END IF
                IF (ixb(i,j,ic1) == 0) THEN
                   dyy = ycn(i,j,1) + (xlm(ic1)-xcn(i,j,1))*dlx - yfg(i,j,ic1)
                   dyy = MIN(0.0,dyy)
                   yfg(i,j,ic1) = yfg(i,j,ic1) + dyy
                END IF
             END IF                                
 
             IF (ic1 == 2) THEN             
                IF (ixb(i,j,ic1) == 1) THEN
                   dxx = xcn(i,j,1) + (ylm(ic1)-ycn(i,j,1))*dly - xfg(i,j,ic1)
                   dxx = MAX(0.0,dxx)
                   xfg(i,j,ic1) = xfg(i,j,ic1) + dxx
                END IF
                IF (ixb(i,j,ic1) == 0) THEN
                   dyy = ycn(i,j,1) + (xlm(ic1)-xcn(i,j,1))*dlx - yfg(i,j,ic1)
                   dyy = MIN(0.0,dyy)
                   yfg(i,j,ic1) = yfg(i,j,ic1) + dyy
                END IF
             END IF                                
 
             IF (ic1 == 3) THEN             
                IF (ixb(i,j,ic1) == 1) THEN
                   dxx = xcn(i,j,1) + (ylm(ic1)-ycn(i,j,1))*dly - xfg(i,j,ic1)
                   dxx = MIN(0.0,dxx)
                   xfg(i,j,ic1) = xfg(i,j,ic1) + dxx
                END IF
                IF (ixb(i,j,ic1) == 0) THEN
                   dyy = ycn(i,j,1) + (xlm(ic1)-xcn(i,j,1))*dlx - yfg(i,j,ic1)
                   dyy = MAX(0.0,dyy)
                   yfg(i,j,ic1) = yfg(i,j,ic1) + dyy
                END IF
             END IF                                
 
             IF (ic1 == 4) THEN             
                IF (ixb(i,j,ic1) == 1) THEN
                   dxx = xcn(i,j,1) + (ylm(ic1)-ycn(i,j,1))*dly - xfg(i,j,ic1)
                   dxx = MAX(0.0,dxx)
                   xfg(i,j,ic1) = xfg(i,j,ic1) + dxx
                END IF
                IF (ixb(i,j,ic1) == 0) THEN
                   dyy = ycn(i,j,1) + (xlm(ic1)-xcn(i,j,1))*dlx - yfg(i,j,ic1)
                   dyy = MAX(0.0,dyy)
                   yfg(i,j,ic1) = yfg(i,j,ic1) + dyy
                END IF
             END IF                                

             IF (ABS(xfg(i,j,ic1)+xlm(ic1)) > 1.) xfg(i,j,ic1) = xlm(ic1)
             IF (ABS(yfg(i,j,ic1)+ylm(ic1)) > 1.) yfg(i,j,ic1) = ylm(ic1)

          END IF                    !NCT=3 and ICLS=1

          IF (nct == 3 .AND. icls == 2) THEN    !NCT=3 and ICLS=2 NCOD=3
             ic1 = 10 - icn(i,j,1) - 2*icn(i,j,2) - 3*icn(i,j,3) - 4*icn(i,j,4)
             yfg(i,j,ic1) = ycn(i,j,2)
             xfg(i,j,ic1) = xcn(i,j,2)
             IF (ABS(xfg(i,j,ic1)+xlm(ic1)) > 1.) xfg(i,j,ic1) = xlm(ic1)
             IF (ABS(yfg(i,j,ic1)+ylm(ic1)) > 1.) yfg(i,j,ic1) = ylm(ic1)
          END IF                    !NCT=3 and ICLS=2
 
          IF (nct == 2 .AND. icls == 2) THEN      !NCT=2 ICLS=2 NCOD=2

             IF (icn(i,j,1)+icn(i,j,2) == 2) THEN   !IT=1 and 2
                xfg(i,j,3) = xlm(3)
                xfg(i,j,4) = xlm(4)
                yfg(i,j,3) = (ycn(i,j,1)+(xlm(3)-xcn(i,j,1))*dlx)
                yfg(i,j,4) = (ycn(i,j,1)+(xlm(4)-xcn(i,j,1))*dlx)
                IF (ABS(yfg(i,j,3)+ylm(3)) > 1.) yfg(i,j,3) = ylm(3)
                IF (ABS(yfg(i,j,4)+ylm(4)) > 1.) yfg(i,j,4) = ylm(4)
             END IF                                 !IT=1 and 2

             IF (icn(i,j,3)+icn(i,j,4) == 2) THEN   !IT=3 and 4
                xfg(i,j,1) = xlm(1)
                xfg(i,j,2) = xlm(2)
                yfg(i,j,1) = (ycn(i,j,1)+(xlm(1)-xcn(i,j,1))*dlx)
                yfg(i,j,2) = (ycn(i,j,1)+(xlm(2)-xcn(i,j,1))*dlx)
                IF (ABS(yfg(i,j,1)+ylm(1)) > 1.) yfg(i,j,1) = ylm(1)
                IF (ABS(yfg(i,j,2)+ylm(2)) > 1.) yfg(i,j,2) = ylm(2)
             END IF                                 !IT=3 and 4

             IF (icn(i,j,2)+icn(i,j,4) == 2) THEN   !IT=2 and 4  
                yfg(i,j,1) = ylm(1)
                yfg(i,j,3) = ylm(3)
                xfg(i,j,1) = (xcn(i,j,1)+(ylm(1)-ycn(i,j,1))*dly)
                xfg(i,j,3) = (xcn(i,j,1)+(ylm(3)-ycn(i,j,1))*dly)
                IF (ABS(xfg(i,j,1)+xlm(1)) > 1.) xfg(i,j,1) = xlm(1)
                IF (ABS(xfg(i,j,3)+xlm(3)) > 1.) xfg(i,j,3) = xlm(3)
             END IF                                 !IT=2 and 4

             IF (icn(i,j,1)+icn(i,j,3) == 2) THEN   !IT=1 and 3
                yfg(i,j,2) = ylm(2)
                yfg(i,j,4) = ylm(4)
                xfg(i,j,2) = (xcn(i,j,1)+(ylm(2)-ycn(i,j,1))*dly)
                xfg(i,j,4) = (xcn(i,j,1)+(ylm(4)-ycn(i,j,1))*dly)
                IF (ABS(xfg(i,j,2)+xlm(2)) > 1.) xfg(i,j,2) = xlm(2)
                IF (ABS(xfg(i,j,4)+xlm(4)) > 1.) xfg(i,j,4) = xlm(4)
             END IF                                 !IT=1 and 3

          END IF                                  !NCT=2 ICLS=2

          IF (nct == 1 .AND. icls == 4) THEN       !NCT=1 ICLS=4 NCOD=3
!    debug section
             ic1 = icn(i,j,1) + 2*icn(i,j,2) + 3*icn(i,j,3) + 4*icn(i,j,4)
             ic2 = 5 - ic1
             ic3 = ixb(i,j,1)*(1-iyb(i,j,1)) + 2*ixb(i,j,2)*(1-iyb(i,j,2)) +  &
                 3*ixb(i,j,3)*(1-iyb(i,j,3)) + 4*ixb(i,j,4)*(1-iyb(i,j,4))
             ic4 = 10 - ic1 - ic2 - ic3
!            WRITE(msg,*)  'ic1,ic2,ic3,ic4=',ic1,ic2,ic3,ic4 
!            call wrf_debug(_FR_DBG_LEVEL_ ,msg)
             iod = ic1 - 2*(ic1/2)
             is = 1 - 2*iod
             js = -1 + 2*(ic1/3)
             isen = is*js
!            WRITE(msg,*)  'iod,is,js,isen=',iod,is,js,isen 
!            call wrf_debug(_FR_DBG_LEVEL_ ,msg)

             x1 = xcn(i2,j2,2) + float(i2-i)
             y1 = ycn(i2,j2,2) + float(j2-j)
             x2 = xcn(i2,j2,1) + float(i2-i)
             y2 = ycn(i2,j2,1) + float(j2-j)
             x3 = xcn(i,j,2+isen)
             y3 = ycn(i,j,2+isen)
             x4 = xcn(i,j,2)
             y4 = ycn(i,j,2)
             x5 = xcn(i,j,2-isen)
             y5 = ycn(i,j,2-isen)
             x6 = xcn(i1,j1,nh1) + FLOAT(i1-i)
             y6 = ycn(i1,j1,nh1) + FLOAT(j1-j)
             x7 = xcn(i1,j1,nh1-1) + FLOAT(i1-i)
             y7 = ycn(i1,j1,nh1-1) + FLOAT(j1-j)
             IF (isen == -1) THEN
                x1 = xcn(i1,j1,nh1-1) + FLOAT(i1-i)
                y1 = ycn(i1,j1,nh1-1) + FLOAT(j1-j)
                x2 = xcn(i1,j1,nh1) + FLOAT(i1-i)
                y2 = ycn(i1,j1,nh1) + FLOAT(j1-j)
                x6 = xcn(i2,j2,1) + FLOAT(i2-i)
                y6 = ycn(i2,j2,1) + FLOAT(j2-j)
                x7 = xcn(i2,j2,2) + FLOAT(i2-i)
                y7 = ycn(i2,j2,2) + FLOAT(j2-j)
!               WRITE(msg,*)  'ad: x1,x2,x3,x4,x5,x6,x7=',x1,x2,x3,x4,x5,x6,x7 
!               call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!               WRITE(msg,*)  'ad: y1,y2,y3,y4,y5,y6,y7=',y1,y2,y3,y4,y5,y6,y7 
!               call wrf_debug(_FR_DBG_LEVEL_ ,msg)
             END IF

! -----  first choice
             xfg_a3 = x7 + (ylm(ic3)-y7)*(x4-x7)/(y4-y7+SIGN(ep,y4-y7))
             yfg_a4 = y1 + (xlm(ic4)-x1)*(y4-y1)/(x4-x1+SIGN(ep,x4-x1))

! -----  second choice
             xfg_b3 = x7 + (ylm(ic3)-y7)*(x6-x7)/(y6-y7+SIGN(ep,y6-y7))
             yfg_b4 = y1 + (xlm(ic4)-x1)*(y2-y1)/(x2-x1+SIGN(ep,x2-x1))

! -----  third choice
             xfg_c3 = x5 + (ylm(ic3)-y5)*(x4-x5)/(y4-y5+SIGN(ep,y4-y5))
             yfg_c4 = y3 + (xlm(ic4)-x3)*(y4-y3)/(x4-x3+SIGN(ep,x4-x3))

! -----  fourth choice
             xfg_d3 = x5
             yfg_d4 = y3

!            WRITE(msg,*)  '1st choice xfg_a3, yfg_a4=',xfg_a3, yfg_a4 
!            call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!            WRITE(msg,*)  '2nd choice xfg_b3, yfg_b4=',xfg_b3, yfg_b4 
!            call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!            WRITE(msg,*)  '3nd choice xfg_c3, yfg_c4=',xfg_c3, yfg_c4 
!            call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!            WRITE(msg,*)  '4th choice xfg_d3, yfg_d4=',xfg_d3, yfg_d4 
!            call wrf_debug(_FR_DBG_LEVEL_ ,msg)

! ----- test constraints
!            tia3 = ((xfg_a3-xfg(i,j,ic3))*(xlm(ic3)-xlm(ic1)) &
!              + ABS((xfg_a3-xfg(i,j,ic3))*(xlm(ic3)-xlm(ic1)))) !must be positive
!            tib3 = ((xfg_b3-xfg(i,j,ic3))*(xlm(ic3)-xlm(ic1)) &
!              + ABS((xfg_b3-xfg(i,j,ic3))*(xlm(ic3)-xlm(ic1)))) !must be positive
!            tic3 = ((xfg_c3-xfg(i,j,ic3))*(xlm(ic3)-xlm(ic1)) &
!              + ABS((xfg_c3-xfg(i,j,ic3))*(xlm(ic3)-xlm(ic1)))) !must be positive
!            tid3 = ((xfg_d3-xfg(i,j,ic3))*(xlm(ic3)-xlm(ic1)) &
!              + ABS((xfg_d3-xfg(i,j,ic3))*(xlm(ic3)-xlm(ic1)))) !must be positive
!
!            iia3 = INT((tia3+ABS(tia3))/(2.*ABS(tia3)+ep)+.5)
!            iib3 = INT((tib3+ABS(tib3))/(2.*ABS(tib3)+ep)+.5)
!            iic3 = INT((tic3+ABS(tic3))/(2.*ABS(tic3)+ep)+.5)
!            iid3 = INT((tid3+ABS(tid3))/(2.*ABS(tid3)+ep)+.5)
 
             iia3 = 0
             iib3 = 0
             iic3 = 0
             iid3 = 0

             IF ( (xfg_a3-xfg(i,j,ic3))*(xlm(ic3)-xlm(ic1))  >=  -ep) iia3 = 1
             IF ( (xfg_b3-xfg(i,j,ic3))*(xlm(ic3)-xlm(ic1))  >=  -ep) iib3 = 1
             IF ( (xfg_c3-xfg(i,j,ic3))*(xlm(ic3)-xlm(ic1))  >=  -ep) iic3 = 1
             IF ( (xfg_d3-xfg(i,j,ic3))*(xlm(ic3)-xlm(ic1))  >=  -ep) iid3 = 1

             iib3 = (1-iia3)*iib3
             iic3 = (1-iia3)*(1-iib3)*iic3
             iid3 = (1-iia3)*(1-iib3)*(1-iic3)*iid3
             iit = iia3 + iib3 + iic3 + iid3
 
!            tja4 = ((yfg_a4-yfg(i,j,ic4))*(ylm(ic4)-ylm(ic1)) &
!              + ABS((yfg_a4-yfg(i,j,ic4))*(ylm(ic4)-ylm(ic1)))) !must be positive
!            tjb4 = ((yfg_b4-yfg(i,j,ic4))*(ylm(ic4)-ylm(ic1)) &
!              + ABS((yfg_b4-yfg(i,j,ic4))*(ylm(ic4)-ylm(ic1)))) !must be positive
!            tjc4 = ((yfg_c4-yfg(i,j,ic4))*(ylm(ic4)-ylm(ic1)) &
!              + ABS((yfg_c4-yfg(i,j,ic4))*(ylm(ic4)-ylm(ic1)))) !must be positive
!            tjd4 = ((yfg_d4-yfg(i,j,ic4))*(ylm(ic4)-ylm(ic1)) &
!              + ABS((yfg_d4-yfg(i,j,ic4))*(ylm(ic4)-ylm(ic1)))) !must be positive
!            jja4 = INT((tja4+ABS(tja4))/(2.*ABS(tja4)+ep)+.5)
!            jjb4 = INT((tjb4+ABS(tjb4))/(2.*ABS(tjb4)+ep)+.5)
!            jjc4 = INT((tjc4+ABS(tjc4))/(2.*ABS(tjc4)+ep)+.5)
!            jjd4 = INT((tjd4+ABS(tjd4))/(2.*ABS(tjd4)+ep)+.5)
 
             jja4 = 0
             jjb4 = 0
             jjc4 = 0
             jjd4 = 0

             IF ( (yfg_a4-yfg(i,j,ic4))*(ylm(ic4)-ylm(ic1))  >=  -ep) jja4 = 1
             IF ( (yfg_b4-yfg(i,j,ic4))*(ylm(ic4)-ylm(ic1))  >=  -ep) jjb4 = 1
             IF ( (yfg_c4-yfg(i,j,ic4))*(ylm(ic4)-ylm(ic1))  >=  -ep) jjc4 = 1
             IF ( (yfg_d4-yfg(i,j,ic4))*(ylm(ic4)-ylm(ic1))  >=  -ep) jjd4 = 1

             jjb4 = (1-jja4)*jjb4
             jjc4 = (1-jja4)*(1-jjb4)*jjc4
             jjd4 = (1-jja4)*(1-jjb4)*(1-jjc4)*jjd4
             jjt = jja4 + jjb4 + jjc4 + jjd4

             IF (iit*jjt == 0) THEN
                iflt = 884
                WRITE(msg,*) 'IC1 2 3 4=',ic1,ic2,ic3,ic4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'XFG_A YFG_A=',xfg_a3,yfg_a4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'XFG_B YFG_B=',xfg_b3,yfg_b4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'XFG_C YFG_C=',xfg_c3,yfg_c4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'XFG_D YFG_D=',xfg_d3,yfg_d4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'TIA3 TJA4=',tia3,tja4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'TIB3 TJB4=',tib3,tjb4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'TIC3 TJC4=',tic3,tjc4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'TID3 TJD4=',tid3,tjd4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'IIA3 JJA4=',iia3,jja4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'IIB3 JJB4=',iib3,jjb4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'IIC3 JJC4=',iic3,jjc4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'IID3 JJD4=',iid3,jjd4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)

                WRITE(msg,*)  'debug:  iit,jjt=',iit,jjt 
              call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'debug:IIT=IIA3+IIB3+IIC3+IID3',iit,iia3,iib3,iic3,iid3 
              call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*)  'debug:JJT=JJA4+JJB4+JJC4+JJD4',jjt,jja4,jjb4,jjc4,jjd4 
              call wrf_debug(_FR_DBG_LEVEL_ ,msg)

                CALL fire_error_debug(i,j,iflt,                        &
                                      time,in1,in2,tign_g,             &
                                      nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                                      ixb,iyb,xfg,yfg,                 &
                                      ifms,ifme, kfms,kfme, jfms,jfme, &
                                      if_st,if_en,jf_st,jf_en)

             END IF

             xfg_ic3 = (FLOAT(iia3)*xfg_a3 + FLOAT(iib3)*xfg_b3 +  &
                        FLOAT(iic3)*xfg_c3 + FLOAT(iid3)*xfg_d3)   &
                   /(FLOAT(iia3+iib3+iic3+iid3)+ep)
             yfg_ic4 = (FLOAT(jja4)*yfg_a4 + FLOAT(jjb4)*yfg_b4 +  &
                        FLOAT(jjc4)*yfg_c4 + FLOAT(jjd4)*yfg_d4)   &
                   /(FLOAT(jja4+jjb4+jjc4+jjd4)+ep)
 
             xfg(i,j,ic3) = xfg_ic3
             yfg(i,j,ic3) = ylm(ic3)
             xfg(i,j,ic4) = xlm(ic4)
             yfg(i,j,ic4) = yfg_ic4
             xfg(i,j,ic2) = xcn(i,j,2)
             yfg(i,j,ic2) = ycn(i,j,2)

             IF (ABS(xfg(i,j,ic3)+xlm(ic3)) > 1.) xfg(i,j,ic3) = xlm(ic3)
             IF (ABS(yfg(i,j,ic4)+ylm(ic4)) > 1.) yfg(i,j,ic4) = ylm(ic4)
             IF (ABS(xfg(i,j,ic2)+xlm(ic2)) > 1.) xfg(i,j,ic2) = xlm(ic2)
             IF (ABS(yfg(i,j,ic2)+ylm(ic2)) > 1.) yfg(i,j,ic2) = ylm(ic2)
 
             IF (ABS(xfg(i,j,ic3)) > .5+ep .OR. ABS(yfg(i,j,ic4)) > .5+ep) THEN
                iflt = 885
                WRITE(msg,*) 'IC1 2 3 4=',ic1,ic2,ic3,ic4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'XFG_A YFG_A=',xfg_a3,yfg_a4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'XFG_B YFG_B=',xfg_b3,yfg_b4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'XFG_C YFG_C=',xfg_c3,yfg_c4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'XFG_D YFG_D=',xfg_d3,yfg_d4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'TIA3 TJA4=',tia3,tja4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'TIB3 TJB4=',tib3,tjb4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'TIC3 TJC4=',tic3,tjc4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'TID3 TJD4=',tid3,tjd4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'IIA3 JJA4=',iia3,jja4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'IIB3 JJB4=',iib3,jjb4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'IIC3 JJC4=',iic3,jjc4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                WRITE(msg,*) 'IID3 JJD4=',iid3,jjd4 
                call wrf_debug(_FR_DBG_LEVEL_ ,msg)

                CALL fire_error_debug(i,j,iflt,                        &
                                      time,in1,in2,tign_g,             &
                                      nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                                      ixb,iyb,xfg,yfg,                 &
                                      ifms,ifme, kfms,kfme, jfms,jfme, &
                                      if_st,if_en,jf_st,jf_en)

             END IF

          END IF                                  !NCT=1 ICLS=4
 
       END IF                    !NFL outer loop
    END DO xl
    END DO yl
  
    RETURN

END SUBROUTINE fire_ds

! =========================================================================

SUBROUTINE fire_igs(ixb,iyb,icn,in1,in2,ncod,time,    & ! incoming
                    xcn,ycn,xcd,ycd,sprdx,sprdy,   &
                    ifms,ifme, kfms,kfme, jfms,jfme,  &
                    if_st,if_en,jf_st,jf_en,          &
!                    nfrx,nfry,                        &
                    nfl,tign_g,xfg,yfg)                 ! inout

! -------------------------------------------------------------------------
! This routine treats new ignitions and the initialization of new grids 
! using the XCN and YCN positions. It preserves symmetry and stability by
! first interogating all ignitors for all cells to be ignited.
!
!  IX               is I index of new ignition grid
!  LY               is L index of new ignition grid
! -------------------------------------------------------------------------

   IMPLICIT NONE

! ----- incoming variables

   INTEGER, INTENT(in) :: ifms,ifme, kfms,kfme, jfms,jfme
   INTEGER, INTENT(in) :: if_st,if_en, jf_st,jf_en
!   INTEGER, INTENT(in) :: nfrx,nfry

   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: ixb,iyb,icn
   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,2 ) :: in1,in2
   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme )   :: ncod

   REAL, INTENT(in) :: time

   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xcn,ycn
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xcd,ycd
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: sprdx,sprdy

! ----- inout variables

   INTEGER, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme ) :: nfl

   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: tign_g
   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xfg,yfg

! ----- local variables

   INTEGER :: i,j
   INTEGER :: is,js
   INTEGER :: ip,jp
   INTEGER :: i1,i2
   INTEGER :: j1,j2
   INTEGER :: nh0,nh1,nh2
   INTEGER :: ix1,ix2,ix3
   INTEGER :: iy1,iy2,iy3
   INTEGER :: jy1,jy2,jy3
   INTEGER :: ixsum,jysum,isum,isumc
   INTEGER :: nct,icls
   INTEGER :: is12,is34,is13,is24
   INTEGER :: ic1,ic2,ic3,ic4
   INTEGER :: iod,isen
   INTEGER :: ihita,ihitb
   INTEGER :: ia4,ja4
   INTEGER :: ia6,ja6
   INTEGER :: ib1,jb1
   INTEGER :: ib3,jb3

   INTEGER, DIMENSION( ifms:ifme,jfms:jfme )   :: nc,icl
   INTEGER, DIMENSION( ifms:ifme,jfms:jfme,4 ) :: ist

   REAL :: dlx,dly
   REAL :: xfg_1,yfg_1
   REAL :: xfg_2,yfg_2
   REAL :: xfg_3,yfg_3
   REAL :: xfg_4,yfg_4
   REAL :: x1,x2,x3,x4,x5,x6,x7
   REAL :: y1,y2,y3,y4,y5,y6,y7
   REAL :: xfga,yfga
   REAL :: xfgb,yfgb
   REAL :: tx,ty
   REAL :: txa,tya
   REAL :: txb,tyb
   REAL :: xfg_ic1,yfg_ic1
   REAL :: xfg_a,yfg_a
   REAL :: xfg_b,yfg_b
   REAL :: xfg_a4,yfg_a4
   REAL :: xfg_a6,yfg_a6
   REAL :: xfg_b1,yfg_b1
   REAL :: xfg_b3,yfg_b3
   REAL :: ti1,ti2,ti3,tia4,tia6,tib1,tib3
   REAL :: tj1,tj2,tj3,tja4,tja6,tjb1,tjb3

   CHARACTER (LEN=256) :: msg

! ----- when deriving fireline coordinates we always keep the fire to our left
 
    DO j = jf_st+1,jf_en-1
    DO i = if_st+1,if_en-1
       nc(i,j)  = icn(i,j,1) + icn(i,j,2) + icn(i,j,3) + icn(i,j,4)
       icl(i,j) = ixb(i,j,1) + ixb(i,j,2) + ixb(i,j,3) + ixb(i,j,4) +  &
                  iyb(i,j,1) + iyb(i,j,2) + iyb(i,j,3) + iyb(i,j,4)         
    END DO
    END DO
   
! ----- preprocessing for loop 2

    DO j = jf_st+1,jf_en-1
    DO i = if_st+1,if_en-1

       IF (nfl(i,j) == 1) THEN

          i1 = in1(i,j,1)
          j1 = in1(i,j,2)
          i2 = in2(i,j,1)
          j2 = in2(i,j,2)
          nh1 = ncod(i1,j1)
          nh2 = ncod(i2,j2)
          nh0 = ncod(i,j)
          ix1 = i + INT(2.*xcn(i,j,1))
          ix2 = i + INT(2.*xcn(i,j,2))
          ix3 = i + INT(2.*xcn(i,j,nh0))
          jy1 = j + INT(2.*ycn(i,j,1))
          jy2 = j + INT(2.*ycn(i,j,2))
          jy3 = j + INT(2.*ycn(i,j,nh0))
          ixsum = ix1 + ix2 + ix3 - 3*i
          jysum = jy1 + jy2 + jy3 - 3*j
          isum = IABS(ixsum) + IABS(jysum)
          nct = nc(i,j)
          icls = icl(i,j)
 
          is12 = 0
          is34 = 0
          is13 = 0
          is24 = 0
          IF (isum > 0) THEN
             IF((nct == 2 .AND. icls == 2) .OR. (nct == 4 .AND. icls == 0)) THEN
                IF (icn(i,j,1)+icn(i,j,2) == 2 .AND. jysum > 0  &
                   .AND. nfl(i,j+1) /= 1 .AND. tign_g(i,j+1) < -10.) is12 = 1
                IF (icn(i,j,3)+icn(i,j,4) == 2 .AND. jysum < 0  &
                   .AND. nfl(i,j-1) /= 1 .AND. tign_g(i,j-1) < -10.) is34 = 1
                IF (icn(i,j,1)+icn(i,j,3) == 2 .AND. ixsum > 0  &
                   .AND. nfl(i+1,j) /= 1 .AND. tign_g(i+1,j) < -10.) is13 = 1
                IF (icn(i,j,2)+icn(i,j,4) == 2 .AND. ixsum < 0  &
                   .AND. nfl(i-1,j) /= 1 .AND. tign_g(i-1,j) < -10.) is24 = 1
             END IF
             IF (nct == 3 .AND. icls == 1) THEN
                IF (icn(i,j,1)+icn(i,j,2) == 2 .AND.       &
                    ixb(i,j,3)+ixb(i,j,4) == 0 .AND.       &
                    jysum > 0 .AND. nfl(i,j+1) /= 1 .AND. &
                    tign_g(i,j+1) < -10.) is12 = 1
                IF (icn(i,j,3)+icn(i,j,4) == 2 .AND.       &
                    ixb(i,j,1)+ixb(i,j,2) == 0 .AND.       &
                    jysum < 0 .AND. nfl(i,j-1) /= 1 .AND. &
                    tign_g(i,j-1) < -10.) is34 = 1
                IF (icn(i,j,1)+icn(i,j,3) == 2 .AND.       &
                    iyb(i,j,2)+iyb(i,j,4) == 0 .AND.       &
                    ixsum > 0 .AND. nfl(i+1,j) /= 1 .AND. &
                    tign_g(i+1,j) < -10.) is13 = 1
                IF (icn(i,j,2)+icn(i,j,4) == 2 .AND.       &
                    iyb(i,j,1)+iyb(i,j,3) == 0 .AND.       &
                    ixsum < 0 .AND. nfl(i-1,j) /= 1 .AND. &
                    tign_g(i-1,j) < -10.) is24 = 1
             END IF
          END IF

          ist(i,j,1) = is12
          ist(i,j,2) = is34
          ist(i,j,3) = is13
          ist(i,j,4) = is24
 
       END IF                !NFL=1 loop
    END DO
    END DO
 
    DO j = jf_st+1,jf_en-1
    DO i = if_st+1,if_en-1

       IF (nfl(i,j) == 1) THEN          !check cell i,j

          is12 = ist(i,j,1)
          is34 = ist(i,j,2)
          is13 = ist(i,j,3)
          is24 = ist(i,j,4)

! ----- the preprocessing of is12 etc allows overwriting or double ignition
! -----the checks on nfl give 2/2 ignition dominance.
 
          IF (is12+is34+is13+is24 > 0)  THEN  !2/2  4/0 and 3/1 ignition
             dly = (xcn(i,j,2)-xcn(i,j,1))/ &
                   (ycn(i,j,2)-ycn(i,j,1) + SIGN(ep,ycn(i,j,2)-ycn(i,j,1)))
             dlx = (ycn(i,j,2)-ycn(i,j,1))/ &
                   (xcn(i,j,2)-xcn(i,j,1) + SIGN(ep,xcn(i,j,2)-ycn(i,j,1)))
 
             IF (is12 == 1) THEN
                xfg_4 = xcn(i,j,1) + (ylm(4)-ycn(i,j,1))*dly
                yfg_4 = ycn(i,j,1) + (xlm(4)-xcn(i,j,1))*dlx
                xfg_3 = xcn(i,j,1) + (ylm(3)-ycn(i,j,1))*dly
                yfg_3 = ycn(i,j,1) + (xlm(3)-xcn(i,j,1))*dlx
 
! ----- igniting a 1/4
                IF (yfg_4 > .5 .AND. yfg_3 < .5 .AND. nfl(i,j+1) /= 1) THEN
                   nfl(i,j+1) = 1
                   tign_g(i,j+1) = time
                   xfg(i,j+1,1) = xfg_4 - 5.*ep
                   yfg(i,j+1,1) = ylm(1)
                   xfg(i,j+1,2) = xlm(2)
                   yfg(i,j+1,2) = ylm(2)
                   xfg(i,j+1,4) = xlm(4)
                   yfg(i,j+1,4) = yfg_4 - 1. + 5.*ep
                   xfg(i,j+1,3) = .5*(xfg(i,j+1,1)+xfg(i,j+1,4))
                   yfg(i,j+1,3) = .5*(yfg(i,j+1,1)+yfg(i,j+1,4))
                END IF

! ----- igniting a 1/4
                IF (yfg_3 > .5 .AND. yfg_4 < .5 .AND. nfl(i,j+1) /= 1) THEN 
                   nfl(i,j+1) = 1
                   tign_g(i,j+1) = time
                   xfg(i,j+1,1) = xlm(1)
                   yfg(i,j+1,1) = ylm(1)
                   xfg(i,j+1,2) = xfg_3 + 5.*ep
                   yfg(i,j+1,2) = ylm(2)
                   xfg(i,j+1,3) = xlm(3)
                   yfg(i,j+1,3) = yfg_3 - 1. + 5.*ep
                   xfg(i,j+1,4) = .5*(xfg(i,j+1,2)+xfg(i,j+1,3))
                   yfg(i,j+1,4) = .5*(yfg(i,j+1,2)+yfg(i,j+1,3))
                END IF

                IF (yfg_3 > .5 .AND. yfg_4 > .5) THEN  ! igniting a 2/2
                   nfl(i,j+1) = 1
                   tign_g(i,j+1) = time
                   xfg(i,j+1,1) = xlm(1)
                   yfg(i,j+1,1) = ylm(1)
                   xfg(i,j+1,2) = xlm(2)
                   yfg(i,j+1,2) = ylm(2)
                   xfg(i,j+1,3) = xlm(3)
                   yfg(i,j+1,3) = yfg_3 - 1. + 5.*ep
                   xfg(i,j+1,4) = xlm(4)
                   yfg(i,j+1,4) = yfg_4 - 1. + 5.*ep
                END IF
             END IF

             IF (is34 == 1) THEN
                xfg_1 = xcn(i,j,1) + (ylm(1)-ycn(i,j,1))*dly
                yfg_1 = ycn(i,j,1) + (xlm(1)-xcn(i,j,1))*dlx
                xfg_2 = xcn(i,j,1) + (ylm(2)-ycn(i,j,1))*dly
                yfg_2 = ycn(i,j,1) + (xlm(2)-xcn(i,j,1))*dlx
 
! ----- igniting a 1/4
                IF (yfg_1 < -.5 .AND. yfg_2 > -.5 .AND. nfl(i,j-1) /= 1) THEN
                   nfl(i,j-1) = 1
                   tign_g(i,j-1) = time
                   xfg(i,j-1,1) = xlm(1)
                   yfg(i,j-1,1) = yfg_1 + 1. - 5.*ep
                   xfg(i,j-1,3) = xlm(3)
                   yfg(i,j-1,3) = ylm(3)
                   xfg(i,j-1,4) = xfg_1 + 5.*ep
                   yfg(i,j-1,4) =  ylm(4)
                   xfg(i,j-1,2) = .5*(xfg(i,j-1,1)+xfg(i,j-1,4))
                   yfg(i,j-1,2) = .5*(yfg(i,j-1,1)+yfg(i,j-1,4))
                END IF

! ----- igniting a 1/4
                IF (yfg_2 < -.5 .AND. yfg_1 > -.5 .AND. nfl(i,j-1) /= 1) THEN
                   nfl(i,j-1) = 1
                   tign_g(i,j-1) = time
                   xfg(i,j-1,2) = xlm(2)
                   yfg(i,j-1,2) = yfg_2 + 1. - 5.*ep
                   xfg(i,j-1,3) = xfg_2 - 5.*ep
                   yfg(i,j-1,3) = ylm(3)
                   xfg(i,j-1,4) = xlm(4)
                   yfg(i,j-1,4) = ylm(4)
                   xfg(i,j-1,1) = .5*(xfg(i,j-1,2)+xfg(i,j-1,3))
                   yfg(i,j-1,1) = .5*(yfg(i,j-1,2)+yfg(i,j-1,3))
                END IF

                IF (yfg_2 < -.5 .AND. yfg_1 < -.5) THEN !igniting a 2/2
                   nfl(i,j-1) = 1
                   tign_g(i,j-1) = time
                   xfg(i,j-1,1) = xlm(1)
                   yfg(i,j-1,1) = yfg_1 + 1. - 5.*ep
                   xfg(i,j-1,2) = xlm(2)
                   yfg(i,j-1,2) = yfg_2 + 1. - 5.*ep
                   xfg(i,j-1,3) = xlm(3)
                   yfg(i,j-1,3) = ylm(3)
                   xfg(i,j-1,4) = xlm(4)
                   yfg(i,j-1,4) = xlm(4)
                END IF
             END IF
 
             IF (is24 == 1) THEN

                xfg_1 = xcn(i,j,1) + (ylm(1)-ycn(i,j,1))*dly
                yfg_1 = ycn(i,j,1) + (xlm(1)-xcn(i,j,1))*dlx
                xfg_3 = xcn(i,j,1) + (ylm(3)-ycn(i,j,1))*dly
                yfg_3 = ycn(i,j,1) + (xlm(3)-xcn(i,j,1))*dlx
 
! ----- igniting a 1/4
                IF (xfg_1 < -.5 .AND. xfg_3 > -.5 .AND. nfl(i-1,j) /= 1) THEN
                   nfl(i-1,j) = 1
                   tign_g(i-1,j) = time
                   xfg(i-1,j,1) = xfg_1 + 1. - 5.*ep
                   yfg(i-1,j,1) = ylm(1)
                   xfg(i-1,j,2) = xlm(2)
                   yfg(i-1,j,2) = ylm(2)
                   xfg(i-1,j,4) = xlm(4)
                   yfg(i-1,j,4) = yfg_1 + 5.*ep
                   xfg(i-1,j,3) = .5*(xfg(i-1,j,1)+xfg(i-1,j,4))
                   yfg(i-1,j,3) = .5*(yfg(i-1,j,1)+yfg(i-1,j,4))
                END IF
  
! ----- igniting a 1/4
                IF (xfg_1 > -.5 .AND. xfg_3 < -.5 .AND. nfl(i-1,j) /= 1) THEN
                   nfl(i-1,j) = 1
                   tign_g(i-1,j) = time
                   xfg(i-1,j,3) = xfg_3 + 1. - 5.*ep
                   yfg(i-1,j,3) = ylm(3)
                   xfg(i-1,j,4) = xlm(4)
                   yfg(i-1,j,4) = ylm(4)
                   xfg(i-1,j,2) = xlm(2)
                   yfg(i-1,j,2) = yfg_3 - 5.*ep
                   xfg(i-1,j,1) = .5*(xfg(i-1,j,2)+xfg(i-1,j,3))
                   yfg(i-1,j,1) = .5*(yfg(i-1,j,2)+yfg(i-1,j,3))
                END IF

                IF (xfg_1 < -.5 .AND. xfg_3 < -.5) THEN   !igniting a 2/2
                   nfl(i-1,j) = 1
                   tign_g(i-1,j) = time
                   xfg(i-1,j,3) = xfg_3+1.-5.*ep
                   yfg(i-1,j,3) = ylm(3)
                   xfg(i-1,j,4) = xlm(4)
                   yfg(i-1,j,4) = ylm(4)
                   xfg(i-1,j,2) = xlm(2)
                   yfg(i-1,j,2) = ylm(2)
                   xfg(i-1,j,1) = xfg_1+1.-5.*ep
                   yfg(i-1,j,1) = ylm(1)
                END IF
             END IF
 
             IF (is13 == 1) THEN

                xfg_2 = xcn(i,j,1) + (ylm(2)-ycn(i,j,1))*dly
                yfg_2 = ycn(i,j,1) + (xlm(2)-xcn(i,j,1))*dlx
                xfg_4 = xcn(i,j,1) + (ylm(4)-ycn(i,j,1))*dly
                yfg_4 = ycn(i,j,1) + (xlm(4)-xcn(i,j,1))*dlx
 
! ----- igniting a 1/4
                IF (xfg_2 > .5 .AND. xfg_4 < .5 .AND. nfl(i+1,j) /= 1) THEN
                   nfl(i+1,j) = 1
                   tign_g(i+1,j) = time
                   xfg(i+1,j,1) = xlm(1)
                   yfg(i+1,j,1) = ylm(1)
                   xfg(i+1,j,2) = xfg_2 - 1. + 5.*ep
                   yfg(i+1,j,2) = ylm(2)
                   xfg(i+1,j,3) = xlm(3)
                   yfg(i+1,j,3) = yfg_2 + 5.*ep
                   xfg(i+1,j,4) = .5*(xfg(i+1,j,2)+xfg(i+1,j,3))
                   yfg(i+1,j,4) = .5*(yfg(i+1,j,2)+yfg(i+1,j,3))
                END IF
 
! ----- igniting a 1/4
                IF (xfg_2 < .5 .AND. xfg_4 > .5 .AND. nfl(i+1,j) /= 1) THEN
                   nfl(i+1,j) = 1
                   tign_g(i+1,j) = time
                   xfg(i+1,j,4) = xfg_4 - 1. + 5.*ep
                   yfg(i+1,j,4) = ylm(4)
                   xfg(i+1,j,3) = xlm(3)
                   yfg(i+1,j,3) = ylm(3)
                   xfg(i+1,j,1) = xlm(1)
                   yfg(i+1,j,1) = yfg_4 - 5.*ep
                   xfg(i+1,j,2) = .5*(xfg(i+1,j,1)+xfg(i+1,j,4))
                   yfg(i+1,j,2) = .5*(yfg(i+1,j,1)+yfg(i+1,j,4))
                END IF
 
                IF (xfg_2 > .5 .AND. xfg_4 > .5) THEN   !igniting a 2/2
                   nfl(i+1,j) = 1
                   tign_g(i+1,j) = time
                   xfg(i+1,j,1) = xlm(1)
                   yfg(i+1,j,1) = ylm(1)
                   xfg(i+1,j,2) = xfg_2 - 1. + 5.*ep
                   yfg(i+1,j,2) = ylm(2)
                   xfg(i+1,j,3) = xlm(3)
                   yfg(i+1,j,3) = ylm(3)
                   xfg(i+1,j,4) = xfg_4 - 1. + 5.*ep
                   yfg(i+1,j,4) = ylm(4)
                END IF
 
             END IF

          END IF                                   !NCT=2 ICLS=2
 
       END IF                           !2/2 4/0 and 3/1 ignition
    END DO
    END DO
 
!new version of 3/1 and 3/2 diagonal ignitions
 
    DO j = jf_st+1,jf_en-1
    DO i = if_st+1,if_en-1

       nct = nc(i,j)
       icls = icl(i,j)

       IF (nfl(i,j) == 1 .AND. nct == 3 .AND. time-tign_g(i,j) > ep) THEN

          i1 = in1(i,j,1)
          j1 = in1(i,j,2)
          i2 = in2(i,j,1)
          j2 = in2(i,j,2)

          nh1 = ncod(i1,j1)
          nh2 = ncod(i2,j2)
          nh0 = ncod(i,j)

          ic1 = 10 - icn(i,j,1) - 2*icn(i,j,2) - 3*icn(i,j,3) - 4*icn(i,j,4)
          ic2 = 5 - ic1
          ic3 = ic1 - 1 + 2*(ic1-(ic1/2)*2)
          ic4 = 10 - ic1 - ic2 - ic3

          x1 = FLOAT(icls-1)*(xcn(i1,j1,nh1-1)+FLOAT(i1-i)) &
             + FLOAT(2-icls)*(xcn(i1,j1,nh1-1)+FLOAT(i1-i))
          y1 = FLOAT(icls-1)*(ycn(i1,j1,nh1-1)+FLOAT(j1-j)) &
             + FLOAT(2-icls)*(ycn(i1,j1,nh1-1)+FLOAT(j1-j))
          x2 = FLOAT(icls-1)*xcn(i,j,2) + FLOAT(2-icls)*xcn(i,j,2)
          y2 = FLOAT(icls-1)*ycn(i,j,2) + FLOAT(2-icls)*ycn(i,j,2)
          x3 = FLOAT(icls-1)*xcn(i,j,2) + FLOAT(2-icls)*xcn(i,j,1)
          y3 = FLOAT(icls-1)*ycn(i,j,2) + FLOAT(2-icls)*ycn(i,j,1)
          x4 = FLOAT(icls-1)*(xcn(i2,j2,2)+FLOAT(i2-i)) &
             + FLOAT(2-icls)*(xcn(i2,j2,2)+FLOAT(i2-i))
          y4 = FLOAT(icls-1)*(ycn(i2,j2,2)+FLOAT(j2-j)) &
             + FLOAT(2-icls)*(ycn(i2,j2,2)+FLOAT(j2-j))
  
          xfga = x1 + (ylm(ic1)-y1)*(x2-x1)/(y2-y1+SIGN(ep,y2-y1))
          xfgb = x3 + (ylm(ic1)-y3)*(x4-x3)/(y4-y3+SIGN(ep,y4-y3))
          yfga = y1 + (xlm(ic1)-x1)*(y2-y1)/(x2-x1+SIGN(ep,x2-x1))
          yfgb = y3 + (xlm(ic1)-x3)*(y4-y3)/(x4-x3+SIGN(ep,x4-x3))

          xfga = xfga + SIGN(ep,xlm(ic1))
          xfgb = xfgb + SIGN(ep,xlm(ic1))
          yfga = yfga + SIGN(ep,ylm(ic1))
          yfgb = yfgb + SIGN(ep,ylm(ic1))

! ----- txa=0 means no ignition possible by the A line
! ----- txa=1 means ignition is possible by the A line

          txa = (xlm(ic1)-xfga)*(xlm(ic3)-xlm(ic1))    !debug
          txb = (xlm(ic1)-xfgb)*(xlm(ic3)-xlm(ic1))
          tya = (ylm(ic1)-yfga)*(ylm(ic4)-ylm(ic1))
          tyb = (ylm(ic1)-yfgb)*(ylm(ic4)-ylm(ic1))

          txa = INT((txa+ABS(txa))/(2.*ABS(txa)+ep)+.5)
          txb = INT((txb+ABS(txb))/(2.*ABS(txb)+ep)+.5)
          tya = INT((tya+ABS(tya))/(2.*ABS(tya)+ep)+.5)
          tyb = INT((tyb+ABS(tyb))/(2.*ABS(tyb)+ep)+.5)
 
! ----- we threshold the limit to .5 cell distance

          txa = .5 * txa * (1.-SIGN(1.,ABS(xfga-xlm(ic1))-.5))   !debug
          txb = .5 * txb * (1.-SIGN(1.,ABS(xfgb-xlm(ic1))-.5))
          tya = .5 * tya * (1.-SIGN(1.,ABS(yfga-ylm(ic1))-.5))
          tyb = .5 * tyb * (1.-SIGN(1.,ABS(yfgb-ylm(ic1))-.5)) 

          is =   1 - 2 * (ic1-(ic1/2)*2)
          js = - 1 + 2 * ((ic1-1)/2)

          IF (txa+txb > 1.-ep .AND. tya+tyb > 1.-ep .AND. &
                                   tign_g(i+is,j+js) < -10.) THEN    !IGNITION

             xfg_ic1 = (txa*xfga+txb*xfgb)/(txa+txb+ep)
             yfg_ic1 = (tya*yfga+tyb*yfgb)/(tya+tyb+ep)
             nfl(i+is,j+js) = 1
             tign_g(i+is,j+js) = time
             xfg(i+is,j+js,ic2) = xlm(ic2)
             yfg(i+is,j+js,ic2) = ylm(ic2)
             xfg(i+is,j+js,ic3) = xlm(ic3)
             yfg(i+is,j+js,ic4) = ylm(ic4)
             yfg(i+is,j+js,ic3) = yfg_ic1 - FLOAT(js)*(1.-5.*ep)   !debug
             xfg(i+is,j+js,ic4) = xfg_ic1 - FLOAT(js)*(1.-5.*ep)   !debug
             xfg(i+is,j+js,ic1) = .5*(xfg(i+is,j+js,ic3)+xfg(i+is,j+js,ic4))
             yfg(i+is,j+js,ic1) = .5*(yfg(i+is,j+js,ic3)+yfg(i+is,j+js,ic4))
 
          END IF                                          ! WE HAVE IGNITION
 
       END IF                                              !NFL=1 NCT=3
    END DO
    END DO
!end new version of 3/1 and 3/2 diagonal ignitions
 
    DO j = jf_st+1,jf_en-1
    DO i = if_st+1,if_en-1

       nct = nc(i,j)
       icls = icl(i,j)

          IF (nfl(i,j) == 1 .AND. nct == 1 .AND. icls == 4 .AND.  &
                                             tign_g(i,j) > ep) THEN  !NCT=1 ICLS=4
! debug section
          tx = MAX(ABS(xcn(i,j,1)),ABS(xcn(i,j,3)))
          ty = MAX(ABS(ycn(i,j,1)),ABS(ycn(i,j,3)))

          IF (tx > .5 .OR. ty > .5) THEN      !1/4 likely
             i1 = in1(i,j,1)
             j1 = in1(i,j,2)
             i2 = in2(i,j,1)
             j2 = in2(i,j,2)
             nh1 = ncod(i1,j1)
             nh2 = ncod(i2,j2)
             nh0 = ncod(i,j)
             ic1 = icn(i,j,1) + 2*icn(i,j,2) + 3*icn(i,j,3) + 4*icn(i,j,4)
             ic2 = 5 - ic1
             ic3 = ixb(i,j,1)*(1-iyb(i,j,1)) + 2*ixb(i,j,2)*(1-iyb(i,j,2)) + &
                 3*ixb(i,j,3)*(1-iyb(i,j,3)) + 4*ixb(i,j,4)*(1-iyb(i,j,4))
             ic4 = 10 - ic1 - ic2 - ic3
             iod = ic1 - 2*(ic1/2)
             is = 1 - 2*iod
             js = - 1 + 2*(ic1/3)
             isen = is*js
             x1 = xcn(i2,j2,2) + FLOAT(i2-i)
             y1 = ycn(i2,j2,2) + FLOAT(j2-j)
             x2 = xcn(i2,j2,1) + FLOAT(i2-i)
             y2 = ycn(i2,j2,1) + FLOAT(j2-j)
             x3 = xcn(i,j,2+isen)
             y3 = ycn(i,j,2+isen)
             x4 = xcn(i,j,2)
             y4 = ycn(i,j,2)
             x5 = xcn(i,j,2-isen)
             y5 = ycn(i,j,2-isen)
             x6 = xcn(i1,j1,nh1) + FLOAT(i1-i)
             y6 = ycn(i1,j1,nh1) + FLOAT(j1-j)
             x7 = xcn(i1,j1,nh1-1) + FLOAT(i1-i)
             y7 = ycn(i1,j1,nh1-1) + FLOAT(j1-j)
             IF (isen == -1) THEN
                x1 = xcn(i1,j1,nh1-1) + FLOAT(i1-i)
                y1 = ycn(i1,j1,nh1-1) + FLOAT(j1-j)
                x2 = xcn(i1,j1,nh1) + FLOAT(i1-i)
                y2 = ycn(i1,j1,nh1) + FLOAT(j1-j)
                x6 = xcn(i2,j2,1) + FLOAT(i2-i)
                y6 = ycn(i2,j2,1) + FLOAT(j2-j)
                x7 = xcn(i2,j2,2) + FLOAT(i2-i)
                y7 = ycn(i2,j2,2) + FLOAT(j2-j)
             END IF
             ihita = 0
             ihitb = 0
             IF (nfl(i-is,j) /= 1) THEN            !NFL(I-IS,J) NE 1
                yfg_a4 = y4 + (xlm(ic3)-x4)*(y5-y4)/(x5-x4+SIGN(ep,x5-x4))
                tj1 = (yfg_a4-ylm(ic3))*(ylm(ic2)-ylm(ic3))
                tj2 = (xlm(ic3)-x5)*(xlm(ic1)-xlm(ic3))
                tj3 = .75 - ABS(yfg_a4-ylm(ic3))
                tja4 = MIN((tj1+ABS(tj1)),(tj2+ABS(tj2)),(tj3+ABS(tj3)))
                ja4 = INT((tja4+ABS(tja4))/(2.*ABS(tja4)+ep)+.5)
 
                xfg_a4 = x4 + (ylm(ic3)-y4)*(x5-x4)/(y5-y4+SIGN(ep,y5-y4))
                ti1 = (xlm(ic3)-xfg_a4)*(xlm(ic1)-xlm(ic3))
                ti2 = (xlm(ic3)-x5)*(xlm(ic1)-xlm(ic3))
                ti3 = .75 - ABS(xfg_a4-xlm(ic3))
                tia4 = MIN((ti1+ABS(ti1)),(ti2+ABS(ti2)),(ti3+ABS(ti3)))
                ia4 = INT((tia4+ABS(tia4))/(2.*ABS(tia4)+ep)+.5)
 
                xfg_a6 = x6 + (ylm(ic3)-y6)*(x7-x6)/(y7-y6+SIGN(ep,y7-y6))
                ti1 = (xfg_a6-xlm(ic3))*(xlm(ic3)-xlm(ic1))
                ti2 = (x6-xlm(ic3))*(xlm(ic3)-xlm(ic1))
                ti3 = .75 - ABS(xfg_a6-xlm(ic3))
                tia6 = MIN((ti1+ABS(ti1)),(ti2+ABS(ti2)),(ti3+ABS(ti3)))
                ia6 = INT((tia6+ABS(tia6))/(2.*ABS(tia6)+ep)+.5)
 
                yfg_a6 = y6 + (xlm(ic3)-x6)*(y7-y6)/(x7-x6+SIGN(ep,x7-x6))
                tj1 = (yfg_a6-ylm(ic3))*(ylm(ic2)-ylm(ic3))
                tj2 = (x6-xlm(ic3))*(xlm(ic3)-xlm(ic1))
                tj3 = .75 - ABS(yfg_a6-ylm(ic3))
                tja6 = MIN((tj1+ABS(tj1)),(tj2+ABS(tj2)),(tj3+ABS(tj3)))
                ja6 = INT((tja6+ABS(tja6))/(2.*ABS(tja6)+ep)+.5)
 
! ----- ia6 takes priority over ia4
! ----- la4 takes priority over la6
                ia4 = (1-ia6)*ia4
                ja6 = (1-ja4)*ja6
                xfg_a = (FLOAT(ia4)*xfg_a4+FLOAT(ia6)*xfg_a6)/(FLOAT(ia4+ia6)+ep)
                yfg_a = (FLOAT(ja4)*yfg_a4+FLOAT(ja6)*yfg_a6)/(FLOAT(ja4+ja6)+ep)
                IF (ia4+ia6 > 0 .AND. ja4+ja6 > 0) THEN
                   nfl(i-is,j) = 1
                   ihita = 1
                   tign_g(i-is,j) = time
                   xfg(i-is,j,ic1) = xlm(ic1)
                   yfg(i-is,j,ic1) = ylm(ic1)
                   xfg(i-is,j,ic3) = xfg_a + FLOAT(is)*(1.-5.*ep)
                   yfg(i-is,j,ic3) = ylm(ic3)
                   xfg(i-is,j,ic4) = xlm(ic4)
                   yfg(i-is,j,ic4) = yfg_a + SIGN(5.*ep,ylm(ic2)-ylm(ic3))
                   xfg(i-is,j,ic2) = .5*(xfg(i-is,j,ic3)+xfg(i-is,j,ic4))
                   yfg(i-is,j,ic2) = .5*(yfg(i-is,j,ic3)+yfg(i-is,j,ic4))
                END IF
!               IF (ihita == 1) THEN
                IF (ihita == 3) THEN
                   WRITE(msg,*) 'debug IHITA results follow' 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug IC1 2 3 4=',ic1,ic2,ic3,ic4 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug I J=',i,j 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug I1 J1=',i1,j1 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug I2 J2=',i2,j2 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug IOD IS JS=',iod,is,js 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug XFG_A4 YFG_A4=',xfg_a4,yfg_a4 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug IA4 JA4=',ia4,ja4 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug XFG_A6 YFG_A6=',xfg_a6,yfg_a6 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug IA6 JA6=',ia6,ja6 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug X1 X2=',x1,x2 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug Y1 Y2=',y1,y2 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug X3 X4 X5=',x3,x4,x5 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug Y3 Y4 Y5=',y3,y4,y5 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug X6 X7=',x6,x7 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug Y6 Y7=',y6,y7 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)

                END IF
             END IF                                !NFL(I-IS,J) NE 1
 
             IF (nfl(i,j-js) /= 1) THEN            !NFL(I,J-JS) NE 1

                yfg_b1 = y1 + (xlm(ic4)-x1)*(y2-y1)/(x2-x1+SIGN(ep,x2-x1))
                tj1 = (yfg_b1-ylm(ic4))*(ylm(ic4)-ylm(ic1))
                tj2 = (y2-ylm(ic4))*(ylm(ic4)-ylm(ic1))
                tj3 = .75 - ABS(yfg_b1-ylm(ic4))
                tjb1 = MIN((tj1+ABS(tj1)),(tj2+ABS(tj2)),(tj3+ABS(tj3)))
                jb1 = INT((tjb1+ABS(tjb1))/(2.*ABS(tjb1)+ep)+.5)
 
                xfg_b1 = x1 + (ylm(ic4)-y1)*(x2-x1)/(y2-y1+SIGN(ep,y2-y1))
                ti1 = (xfg_b1-xlm(ic4))*(xlm(ic2)-xlm(ic4))
                ti2 = (y2-ylm(ic4))*(ylm(ic4)-ylm(ic1))
                ti3 = .75 - ABS(xfg_b1-xlm(ic4))
                tib1 = MIN((ti1+ABS(ti1)),(ti2+ABS(ti2)),(ti3+ABS(ti3)))
                ib1 = INT((tib1+ABS(tib1))/(2.*ABS(tib1)+ep)+.5)
 
                xfg_b3 = x3 + (ylm(ic4)-y3)*(x4-x3)/(y4-y3+SIGN(ep,y4-y3))
                ti1 = (xfg_b3-xlm(ic4))*(xlm(ic2)-xlm(ic4))
                ti2 = (y3-ylm(ic4))*(ylm(ic4)-ylm(ic1))
                ti3 = .75 - ABS(xfg_b3-xlm(ic4))
                tib3 = MIN((ti1+ABS(ti1)),(ti2+ABS(ti2)),(ti3+ABS(ti3)))
                ib3 = INT((tib3+ABS(tib3))/(2.*ABS(tib3)+ep)+.5)
 
                yfg_b3 = y3 + (xlm(ic4)-x3)*(y4-y3)/(x4-x3+SIGN(ep,x4-x3))
                tj1 = (yfg_b3-ylm(ic4))*(ylm(ic4)-ylm(ic1))
                tj2 = (y3-ylm(ic4))*(ylm(ic4)-ylm(ic1))
                tj3 = .75 - ABS(yfg_b3-ylm(ic4))
                tjb3 = MIN((tj1+ABS(tj1)),(tj2+ABS(tj2)),(tj3+ABS(tj3)))
                jb3 = INT((tjb3+ABS(tjb3))/(2.*ABS(tjb3)+ep)+.5)
 
! ----- ib3 takes priority over ib1
! ----- jb1 takes priority over jb3
                ib1 = (1-ib3)*ib1
                jb3 = (1-jb1)*jb3
                xfg_b = (FLOAT(ib1)*xfg_b1+FLOAT(ib3)*xfg_b3)/(FLOAT(ib1+ib3)+ep)
                yfg_b = (FLOAT(jb1)*yfg_b1+FLOAT(jb3)*yfg_b3)/(FLOAT(jb1+jb3)+ep)
                IF (ib1+ib3 > 0 .AND. jb1+jb3 > 0) THEN
                   nfl(i,j-js) = 1
                   ihitb = 1
                   tign_g(i,j-js) = time
                   xfg(i,j-js,ic1) = xlm(ic1)
                   yfg(i,j-js,ic1) = ylm(ic1)
                   xfg(i,j-js,ic3) = xfg_b + SIGN(5.*ep,xlm(ic2)-xlm(ic4))
                   yfg(i,j-js,ic3) = ylm(ic3)
                   xfg(i,j-js,ic4) = xlm(ic4)
                   yfg(i,j-js,ic4) = yfg_b + FLOAT(js)*(1.-5.*ep)
                   xfg(i,j-js,ic2) = .5*(xfg(i,j-js,ic3)+xfg(i,j-js,ic4))
                   yfg(i,j-js,ic2) = .5*(yfg(i,j-js,ic3)+yfg(i,j-js,ic4))
                END IF
!               IF (ihitb == 1) THEN
                IF (ihitb == 3) THEN
                   WRITE(msg,*) 'debug IHITB results follow' 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug IC1 2 3 4=',ic1,ic2,ic3,ic4 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug I J=',i,j 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug I1 J1=',i1,j1 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug I2 J2=',i2,j2 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug IOD IS JS=',iod,is,js 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug XFG_B1 YFG_B1=',xfg_b1,yfg_b1 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug XFG_B3 YFG_B3=',xfg_b3,yfg_b3 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug X1 X2=',x1,x2 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug Y1 Y2=',y1,y2 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug X3 X4 X5=',x3,x4,x5 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug Y3 Y4 Y5=',y3,y4,y5 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug X6 X7=',x6,x7 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                   WRITE(msg,*) 'debug Y6 Y7=',y6,y7 
                   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
                END IF
             END IF                             !NFL(I,J-JS) NE 1
          END IF                                !1/4 likely
       END IF                                   !NCT=1 ICLS=4
! debug section
    END DO
    END DO
 
! ----- redundant check for unignited cells - likely missed by diagonal 1/3 or 2/3
 
    DO j = jf_st+1,jf_en-1
    DO i = if_st+1,if_en-1

       isum = nfl(i+1,j) + nfl(i-1,j) + nfl(i,j+1) + nfl(i,j-1)

       IF (tign_g(i,j) < -10. .AND. isum == 2) THEN  !unignited cell
 
! ----- corner = 1

          isumc = icn(i-1,j,2) + icn(i,j-1,3)
          isum = iyb(i-1,j,4) + ixb(i,j-1,4)
          IF (isumc == 2 .AND. isum >= 1) THEN
             WRITE(msg,*) 'debug CN=1 redund igs hit at I J=',i,j 
             call wrf_debug(_FR_DBG_LEVEL_ ,msg)
             tign_g(i,j) = time
             nfl(i,j) = 1
             x1 = xfg(i,j-1,4)
             y1 = yfg(i,j-1,4)-1.
             x2 = xfg(i-1,j,4)-1.
             y2 = yfg(i-1,j,4)
             xfg_2 = x1 + (ylm(2)-y1)*(x2-x1)/(y2-y1+SIGN(ep,y2-y1))
             yfg_3 = y1 + (xlm(3)-x1)*(y2-y1)/(x2-x1+SIGN(ep,x2-x1))
             xfg(i,j,1) = xlm(1)
             yfg(i,j,1) = ylm(1)
             xfg(i,j,2) = xfg_2
             yfg(i,j,2) = ylm(2)
             xfg(i,j,3) = xlm(3)
             yfg(i,j,3) = yfg_3
             xfg(i,j,4) = .5*(xfg(i,j,2)+xfg(i,j,3))
             yfg(i,j,4) = .5*(yfg(i,j,2)+yfg(i,j,3))
          END IF
 
! ----- corner = 2

          isumc = icn(i+1,j,1) + icn(i,j-1,4)
          isum = iyb(i+1,j,3) + ixb(i,j-1,3)
          IF (isumc == 2 .AND. isum >= 1) THEN
             WRITE(msg,*) 'debug CN=2 redund igs hit at I J=',i,j 
             call wrf_debug(_FR_DBG_LEVEL_ ,msg)
             tign_g(i,j) = time
             nfl(i,j) = 1
             x1 = xfg(i+1,j,3) + 1.
             y1 = yfg(i+1,j,3)
             x2 = xfg(i,j-1,3)
             y2 = yfg(i,j-1,3) - 1.
             xfg_1 = x1 + (ylm(1)-y1)*(x2-x1)/(y2-y1+SIGN(ep,y2-y1))
             yfg_4 = y1 + (xlm(4)-x1)*(y2-y1)/(x2-x1+SIGN(ep,x2-x1))
             xfg(i,j,2) = xlm(2)
             yfg(i,j,2) = ylm(2)
             xfg(i,j,1) = xfg_1
             yfg(i,j,1) = ylm(1)
             xfg(i,j,4) = xlm(4)
             yfg(i,j,4) = yfg_4
             xfg(i,j,3) = .5*(xfg(i,j,1)+xfg(i,j,4))
             yfg(i,j,3) = .5*(yfg(i,j,1)+yfg(i,j,4))
          END IF
 
! ----- corner = 3

          isumc = icn(i-1,j,4) + icn(i,j+1,1)
          isum = iyb(i-1,j,2) + ixb(i,j+1,2)
          IF (isumc == 2 .AND. isum >= 1) THEN
             WRITE(msg,*) 'debug CN=3 redund igs hit at I J=',i,j 
             call wrf_debug(_FR_DBG_LEVEL_ ,msg)
             tign_g(i,j) = time
             nfl(i,j) = 1
             x1 = xfg(i-1,j,2) - 1.
             y1 = yfg(i-1,j,2)
             x2 = xfg(i,j+1,2)
             y2 = yfg(i,j+1,2) + 1.
             xfg_4 = x1 + (ylm(4)-y1)*(x2-x1)/(y2-y1+SIGN(ep,y2-y1))
             yfg_1 = y1 + (xlm(1)-x1)*(y2-y1)/(x2-x1+SIGN(ep,x2-x1))
             xfg(i,j,3) = xlm(3)
             yfg(i,j,3) = ylm(3)
             xfg(i,j,4) = xfg_4
             yfg(i,j,4) = ylm(4)
             xfg(i,j,1) = xlm(1)
             yfg(i,j,1) = yfg_1
             xfg(i,j,2) = .5*(xfg(i,j,1)+xfg(i,j,4))
             yfg(i,j,2) = .5*(yfg(i,j,1)+yfg(i,j,4))
          END IF
 
! ----- corner = 4
          isumc = icn(i+1,j,3) + icn(i,j+1,2)
          isum = iyb(i+1,j,1) + ixb(i,j+1,1)
          IF (isumc == 2 .AND. isum >= 1) THEN
             WRITE(msg,*) 'debug CN=4 redund igs hit at I J=',i,j 
             call wrf_debug(_FR_DBG_LEVEL_ ,msg)
             tign_g(i,j) = time
             nfl(i,j) = 1
             x1 = xfg(i,j+1,1)
             y1 = yfg(i,j+1,1)+1.
             x2 = xfg(i+1,j,1)+1.
             y2 = yfg(i+1,j,1)
             xfg_3 = x1 + (ylm(3)-y1)*(x2-x1)/(y2-y1+SIGN(ep,y2-y1))
             yfg_2 = y1 + (xlm(2)-x1)*(y2-y1)/(x2-x1+SIGN(ep,x2-x1))
             xfg(i,j,4) = xlm(4)
             yfg(i,j,4) = ylm(4)
             xfg(i,j,3) = xfg_3
             yfg(i,j,3) = ylm(3)
             xfg(i,j,2) = xlm(2)
             yfg(i,j,2) = yfg_2
             xfg(i,j,1) = .5*(xfg(i,j,2)+xfg(i,j,3))
             yfg(i,j,1) = .5*(yfg(i,j,2)+yfg(i,j,3))
          END IF
 
       END IF                                       !unignited cell
    END DO
    END DO

    RETURN

END SUBROUTINE fire_igs

! =========================================================================

SUBROUTINE fire_burn_fcn(i,j,                              & ! incoming
                         nfuel_cat,nfl,ncod,in1,in2,       &
                         ixb,iyb,icn,time,area2,           &
                         tign_g,tign_crt,                  &
                         xcd,ycd,xcn,ycn,xfg,yfg,          &
                         ifms,ifme, kfms,kfme, jfms,jfme,  &
                         if_st,if_en,jf_st,jf_en,          &
                         burn_frac)                          ! outgoing
 
! -----------------------------------------------------------------------
!     This subroutine gives a fit to the mass loss curve.
!        It calculates the fraction of mass left in the cell's ignited
!        area at the time given.
!        The function approximates a decreasing exponential with
!        weighting value WEIGHT ranging from 20 (fast burnup) to 1000
!        ( ~40% decrease in mass over 10 min).
!     BURN_FRAC= fraction of fuel mass in cell that has been burned in
!       last timestep.
!     TIGN_CRT(I,L): if fuel cell is not fully ignited, it is negative.
!       If fuel cell fully ignited, it is dimensionless (universal) time
!     TCELL :  time since cell ignition (s)
!     TMCRIT : first time since cell ignition that whole cell is burning (s)
! -----------------------------------------------------------------------

   USE module_fr_cawfe_fuel

   IMPLICIT NONE

! ------ incoming variables

   INTEGER, INTENT(in) :: ifms,ifme, kfms,kfme, jfms,jfme
   INTEGER, INTENT(in) :: if_st,if_en, jf_st,jf_en

   INTEGER, INTENT(in) :: i,j

   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme )   :: nfuel_cat
   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme )   :: nfl,ncod
   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,2 ) :: in1,in2
   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: ixb,iyb,icn

   REAL, INTENT(in) :: time

   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme )   :: area2
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme )   :: tign_g,tign_crt
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xcd,ycd
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xcn,ycn
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: xfg,yfg

! ------ outgoing variables

   REAL, INTENT(out) :: burn_frac

! ------ local variables

   INTEGER :: iflt

   REAL :: are1
   REAL :: tcell
   REAL :: burn_fracc
   REAL :: tmcrit

   CHARACTER(LEN=256) :: msg

! --------------------------------------------------------------------------

   are1 = area2(i,j)

! ----- CVMGP: returns x1 if x3 >=0, x2 if x3 < 0
   tcell = cvmgp( time-tign_g(i,j), 0., tign_g(i,j) )  
 
   IF (tign_crt(i,j) < 0.) THEN   ! fuel cell not yet fully burning
 
      IF (tcell > ep) THEN
         burn_frac = are1 + are1*weight(nfuel_cat(i,j))/tcell &
                   * (EXP(-tcell/weight(nfuel_cat(i,j)))-1.)
      ELSE
         burn_frac = 0.
      END IF

   ELSE                              !fuel cell is fully burning

      tmcrit = tign_crt(i,j) - tign_g(i,j)

      IF (tmcrit > ep) THEN

         burn_fracc = are1 + are1*weight(nfuel_cat(i,j))/tmcrit  &
                    *(EXP(-tmcrit/weight(nfuel_cat(i,j)))-1.)

         burn_frac = burn_fracc + weight(nfuel_cat(i,j))/tmcrit  &
                 *(1.-EXP(-tmcrit/weight(nfuel_cat(i,j))))       &
                 *(1.-EXP( (tmcrit-tcell)/weight(nfuel_cat(i,j)) ))

      ELSE   ! fuel cell is ~instantly fully lit (kin test)

         burn_frac = 1.-EXP(-tmcrit/weight(nfuel_cat(i,j))) ! test: point burning

         IF (tign_crt(i,j) > ep .AND. ABS(time-tign_crt(i,j)) < ep)  &
                  THEN    !avoid initialization cases

            WRITE(msg,66) tmcrit,burn_frac,i,j,time 
            call wrf_debug(_FR_DBG_LEVEL_ ,msg)
  66        FORMAT(1X,'Warning 215- TMCRIT BURN_FRAC =',2F5.2,   &
                  ', I J =',2I4,', TIME =',F8.2)
         END IF
      END IF
!
      IF (tmcrit > tcell) THEN 
        WRITE(msg,*)  'STOP 213: BURN_FRAC,ARE1,WEIGHT,TCELL,TMCRIT=', &
        i,j,burn_frac,are1,weight(nfuel_cat(i,j)),tcell,tmcrit
         call wrf_debug(_FR_DBG_LEVEL_ ,msg)
         iflt = 213 
         CALL fire_error_debug(i,j,iflt,                        &
                               time,in1,in2,tign_g,             &
                               nfl,ncod,xcd,ycd,xcn,ycn,icn,    &
                               ixb,iyb,xfg,yfg,                 &
                               ifms,ifme, kfms,kfme, jfms,jfme, &
                               if_st,if_en,jf_st,jf_en)
      END IF                    
   END IF

   RETURN

END SUBROUTINE fire_burn_fcn

! =========================================================================

SUBROUTINE fire_ros( i,j,                             &    ! incoming
                     ifms,ifme, kfms,kfme, jfms,jfme, &
                     speed,tanphi,ibeh,nfuel_cat,     &
                     bbb,phiwc,betafl,r_0,            &
                     ros)                                 ! outgoing
 
!-----------------------------------------------------------------------
!     ... calculates the rate of fire spread with mcarthur formula or behave
!           using fuel type of fuel cell
!         
!     m/s = (ft/min) *.3048/60. = (ft/min) * .00508 ! conversion rate
!     ft/min = m/s * 2.2369 * 88. = m/s *  196.850  ! conversion rate
!         
!-----------------------------------------------------------------------

   USE module_fr_cawfe_fuel
 
   IMPLICIT NONE

! ------ incoming variables

   INTEGER, INTENT(in) :: ifms,ifme, kfms,kfme, jfms,jfme
   INTEGER, INTENT(in) :: ibeh
   INTEGER, INTENT(in) :: i,j

   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme ) :: nfuel_cat

   REAL, INTENT(in) :: speed,tanphi

   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme ) :: bbb,phiwc,betafl,r_0

! ------ in and out going variables


! ------ outgoing variables

   REAL, INTENT(out) :: ros

! ------ local variables

   REAL :: spd_ms,spd_fm
   REAL :: phis,phiw,sp_n
   CHARACTER(LEN=96) :: msg
 
! ------ begin routine

   IF (ichap(nfuel_cat(i,j)) == 0) THEN  ! not chaparral 

      IF (ibeh == 1) THEN                ! BEHAVE

! --- if wind is 0 or into fireline, phiw=0, this reduces to backing r.o.s.

         spd_ms = .5*(speed + ABS(speed)) 
         spd_ms = MIN(spd_ms,10.)          ! max input wind spd is 10 m/s  !param

         spd_fm = spd_ms * 196.850         ! convert wind spd from m/s to ft/min

! ----- wind factor : phiw = c * spd_fm**bbb(i,j) * (betafl(i,j)/betaop)**(-e)

         phiw = spd_fm**bbb(i,j) * phiwc(i,j) ! wind coef  

! --- slope factor

         phis = 0.
         IF (tanphi  >  0.) THEN
            phis = 5.275 *(betafl(i,j))**(-0.3) *tanphi**2   ! slope factor 
         END IF

! --- spread rate (m/s)

         ros = r_0(i,j) * (1. + phiw + phis) * .00508

         WRITE (msg,'(x,a,2i4,5(x,e12.3) )') 'i,j,ros,r0,phiw,phis=',  &
                                              i,j,ros,r_0(i,j),phiw,phis
         CALL wrf_message( msg )

         IF (ros  >  1.) WRITE(msg,*)  'speed=',speed,' tanphi=',tanphi 
              call wrf_debug(_FR_DBG_LEVEL_ ,msg)
 
      ELSE IF (ibeh == 0) THEN                              !MACARTHUR FORMULA

         ros = 0.18 * EXP(0.8424*.5*(speed+ABS(speed))) 

      END IF
 
   ELSE IF (ichap(nfuel_cat(i,j)) == 1) THEN   ! chaparral
!        .... spread rate has no dependency on fuel character, only windspeed.
      spd_ms = .5*(speed+ABS(speed))
      ros = 1.2974 * spd_ms**1.41       ! spread rate, m/s
! -- note: backing r.o.s. is 0 for chaparral without setting nozero value below
      sp_n =.03333           ! Chaparral backing fire spread rate 0.033 m/s
      ros = MAX(ros, sp_n)   ! no less than backing r.o.s.
   END IF
 
!     ----------NOTE!  Put an 6 m/s cap on max spread rate -----------

   ros = MIN(ros, 6.)         ! no faster than this cap   ! param ! 

!  IF (spd_ms  >  0) THEN
!     WRITE (6,120) i,j,'  spd_ms,ros =',spd_ms,ros,'  phiw,r0_ms=',  &
!          phiw, r_0(i,j)*0.00508
!  END IF
!120 FORMAT (1x,2i3,a,2f10.4,a,2f10.4)

   RETURN

END SUBROUTINE fire_ros

! =========================================================================

SUBROUTINE fire_valid14(i,j,ic1,ic2,ic3,ic4,ita,itb,itc,nc, &
                        icl,xfg,yfg,ixb,iyb,icn,            &
                        ifms,ifme, kfms,kfme, jfms,jfme)

! ----- routine checks the validity of fire points

   IMPLICIT NONE

! ----- incoming variables

   INTEGER, INTENT(in) :: ifms,ifme, kfms,kfme, jfms,jfme
   INTEGER, INTENT(in) :: i,j

! ----- inout variables

   INTEGER, INTENT(inout) :: ic1,ic2,ic3,ic4
   INTEGER, INTENT(inout) :: ita,itb,itc

   INTEGER, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 ) :: ixb,iyb,icn
   INTEGER, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme )   :: nc,icl

   REAL, INTENT(inout), DIMENSION( ifms:ifme,jfms:jfme,4 )  :: xfg,yfg

! ----- local variables

   INTEGER :: iod
   INTEGER :: is,js

! ----- begin routine

    ic1 = icn(i,j,1) + 2*icn(i,j,2) + 3*icn(i,j,3) + 4*icn(i,j,4)
    ic2 = 5 - ic1
    ic3 = ixb(i,j,1)*(1-iyb(i,j,1)) + 2*ixb(i,j,2)*(1-iyb(i,j,2)) + &
        3*ixb(i,j,3)*(1-iyb(i,j,3)) + 4*ixb(i,j,4)*(1-iyb(i,j,4))
    ic4 = 10 - ic1 - ic2 - ic3
    iod = ic1 - 2*(ic1/2)
    is = 1 - 2*iod
    js = -1 + 2*(ic1/3)
    ita = icn(i-is,j,ic1) + icn(i-is,j+js,ic4)
    itb = icn(i+is,j-js,ic3) + icn(i,j-js,ic1)
    itc = icn(i-is,j,ic4) + icn(i-is,j-js,ic1) + icn(i,j-js,ic3)

    IF (ita > 0) then
       xfg(i,j,ic3) = xlm(ic3)
       yfg(i,j,ic3) = ylm(ic3)
       icn(i,j,ic3) = 1
       ixb(i,j,ic3) = 0
       iyb(i,j,ic3) = 0
       xfg(i,j,ic2) = xlm(ic2)
       yfg(i,j,ic2) = ylm(ic3) + 5.*ep*(ylm(ic2)-ylm(ic3))
    END IF

    IF (itb > 0) THEN
       xfg(i,j,ic4) = xlm(ic4)
       yfg(i,j,ic4) = ylm(ic4)
       icn(i,j,ic4) = 1
       ixb(i,j,ic4) = 0
       iyb(i,j,ic4) = 0
       xfg(i,j,ic2) = xlm(ic4) + 5.*ep*(xlm(ic2)-xlm(ic4))
       yfg(i,j,ic2) = ylm(ic2)
    END IF

    IF (itc > 0) THEN
       xfg(i,j,ic2) = xlm(ic2)
       yfg(i,j,ic2) = ylm(ic2)
       icn(i,j,ic2) = 1
       ixb(i,j,ic2) = 0
       iyb(i,j,ic2) = 0
    END IF
 
! ??????
    IF (ita > 0 .AND. itb > 0 .AND. itc == 0) THEN
! ----- chose IC2 coordinate positions
    END IF

    nc(i,j)  = icn(i,j,1) + icn(i,j,2) + icn(i,j,3) + icn(i,j,4)
    icl(i,j) = ixb(i,j,1) + ixb(i,j,2) + ixb(i,j,3) + ixb(i,j,4) + &
               iyb(i,j,1) + iyb(i,j,2) + iyb(i,j,3) + iyb(i,j,4)
 
    RETURN

END SUBROUTINE fire_valid14

! =========================================================================

SUBROUTINE fire_error_debug(i,j,iflt,time,in1,in2,tign_g,          &
                            nfl,ncod,xcd,ycd,xcn,ycn,icn,ixb,iyb,  &
                            xfg,yfg,                               &
                            ifms,ifme, kfms,kfme, jfms,jfme,       &
                            if_st,if_en,jf_st,jf_en)

! ------- this routine writes debug to stdout and to ncar graphics

    USE module_wrf_error

    IMPLICIT NONE

! ------- incoming variables

   INTEGER, INTENT(in) :: ifms,ifme, kfms,kfme, jfms,jfme
   INTEGER, INTENT(in) :: if_st,if_en, jf_st,jf_en

   INTEGER, INTENT(in) :: i,j,iflt
 
 
   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme )    :: nfl,ncod
   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme, 2 ) :: in1,in2
   INTEGER, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme, 4 ) :: ixb,iyb,icn
 
   REAL, INTENT(in) :: time

   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme )       :: tign_g
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme, 4 )    :: xcd,ycd
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme, 4 )    :: xcn,ycn
   REAL, INTENT(in), DIMENSION( ifms:ifme,jfms:jfme, 4 )    :: xfg,yfg

! ------ local variables

   INTEGER, PARAMETER :: ierrf = 6
   INTEGER, PARAMETER :: lunit = 2
   INTEGER, PARAMETER :: iwkid = 1 

   INTEGER :: ii,jj,it,ic,ierr,nct,icls,ixn,jyn,i1,j1,iszdm
   INTEGER :: idl,jdl,i2,j2,iix,jjy,npd
  
   INTEGER, DIMENSION( ifms:ifme,jfms:jfme ) :: nc,icc

   REAL :: txn,tyn,x1,x2,y1,y2,xx1,yy1,xx2,yy2,xx3,yy3,dl,rwidth

   CHARACTER (LEN=4)   :: flitnu
   CHARACTER (LEN=80)  :: lhead
   CHARACTER (LEN=80)  :: cdum
   CHARACTER (LEN=21)  :: fnmi
   CHARACTER (LEN=5)   :: iflg
   CHARACTER (LEN=256) :: msg 

! ---------- end declarations, begin processing
 
!jbeezley : this whole block of code needs to be fixed up :
!           skipping for now

   GOTO 9999

   DO jj = jf_st,jf_en
   DO ii = if_st,if_en
      nc(ii,jj) = 0
      icc(ii,jj) = 0
      DO it=1,4
         nc(ii,jj)  = nc(ii,jj)  + icn(ii,jj,it)
         icc(ii,jj) = icc(ii,jj) + ixb(ii,jj,it) + iyb(ii,jj,it)
      END DO
   END DO
   END DO

   WRITE(msg,11) time,i,j,iflt 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
11 FORMAT(1x,'ERROR DEBUG AT TIME=',f9.3,' I J=',2i4,' IFLT=',i4)

   WRITE(msg,80) i,j,ncod(i,j),(in1(i,j,ic),ic=1,2),(in2(i,j,ic),ic=1,2) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
80 FORMAT(1x,'I J NCOD=',3i4/4x,'IN1=',2i4/4x,'IN2=',2i4)
 
   WRITE(msg,84) ((time-tign_g(ii,jj),ii=i-1,i+1),jj=j+1,j-1,-1) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
84 FORMAT(1x,'TIME-TIGN_G=',3f14.4)
 
   WRITE(msg,85) ((nfl(ii,jj),ii=i-1,i+1),jj=j+1,j-1,-1) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
85 FORMAT(1x,'  NFL      =',3i10)
   WRITE(msg,135) ((nc(ii,jj),ii=i-1,i+1),jj=j+1,j-1,-1) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
135 FORMAT(1X,'  NC       =',3i10)
   WRITE(msg,136) ((icc(ii,jj),ii=i-1,i+1),jj=j+1,j-1,-1) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
136 FORMAT(1X,'  ICLS     =',3i10)
 
   WRITE(msg,63) (xcd(i,j,it),it=1,4),(ycd(i,j,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
63 FORMAT(1x,'XCD(I  J )=',4f10.7/1x,'YCD(I  J =',4f10.7)
   WRITE(msg,67) (xcd(i+1,j,it),it=1,4),(ycd(i+1,j,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
67 FORMAT(1x,'XCD(IP J )=',4f10.7/1x,'YCD(IP J =',4f10.7)
   WRITE(msg,68) (xcd(i-1,j,it),it=1,4),(ycd(i-1,j,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
68 FORMAT(1x,'XCD(IM J )=',4f10.7/1x,'YCD(IM J =',4f10.7)
   WRITE(msg,69) (xcd(i,j+1,it),it=1,4),(ycd(i,j+1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
69 FORMAT(1x,'XCD(I  JP)=',4f10.7/1x,'YCD(I  JP=',4f10.7)
   WRITE(msg,70) (xcd(i,j-1,it),it=1,4),(ycd(i,j-1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
70 FORMAT(1x,'XCD(I  JM)=',4f10.7/1x,'YCD(I  JM=',4f10.7)
   WRITE(msg,71) (xcd(i+1,j+1,it),it=1,4),(ycd(i+1,j+1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
71 FORMAT(1x,'XCD(IP JP)=',4f10.7/1x,'YCD(IP JP=',4f10.7)
   WRITE(msg,72) (xcd(i-1,j-1,it),it=1,4),(ycd(i-1,j-1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
72 FORMAT(1x,'XCD(IM JM)=',4f10.7/1x,'YCD(IM JM=',4f10.7)
   WRITE(msg,79) (xcd(i+1,j-1,it),it=1,4),(ycd(i+1,j-1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
79 FORMAT(1x,'XCD(IP JM)=',4f10.7/1x,'YCD(IP JM=',4f10.7)
   WRITE(msg,83) (xcd(i-1,j+1,it),it=1,4),(ycd(i-1,j+1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
83 FORMAT(1x,'XCD(IM JP)=',4f10.7/1x,'YCD(IM JP=',4f10.7)
 
   WRITE(msg,65) (xcn(i,j,it),it=1,4),(ycn(i,j,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
65 FORMAT(1x,'XCN(I  J =',4f10.7/1x,'YCN(I  J =',4f10.7)
   WRITE(msg,73) (xcn(i+1,j,it),it=1,4),(ycn(i+1,j,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
73 FORMAT(1x,'XCN(IP J =',4f10.7/1x,'YCN(IP J =',4f10.7)
   WRITE(msg,74) (xcn(i-1,j,it),it=1,4),(ycn(i-1,j,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
74 FORMAT(1x,'XCN(IM J =',4f10.7/1x,'YCN(IM J =',4f10.7)
   WRITE(msg,75) (xcn(i,j+1,it),it=1,4),(ycn(i,j+1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
75 FORMAT(1x,'XCN(I  JP=',4f10.7/1x,'YCN(I  JP=',4f10.7)
   WRITE(msg,76) (xcn(i,j-1,it),it=1,4),(ycn(i,j-1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
76 FORMAT(1x,'XCN(I  JM=',4f10.7/1x,'YCN(I  JM=',4f10.7)
   WRITE(msg,77) (xcn(i+1,j+1,it),it=1,4),(ycn(i+1,j+1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
77 FORMAT(1x,'XCN(IP JP=',4f10.7/1x,'YCN(IP JP=',4f10.7)
   WRITE(msg,78) (xcn(i-1,j-1,it),it=1,4),(ycn(i-1,j-1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
78 FORMAT(1x,'XCN(IM JM=',4f10.7/1x,'YCN(IM JM=',4f10.7)
   WRITE(msg,81) (xcn(i+1,j-1,it),it=1,4),(ycn(i+1,j-1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
81 FORMAT(1x,'XCN(IP JM=',4f10.7/1x,'YCN(IP JM=',4f10.7)
   WRITE(msg,82) (xcn(i-1,j+1,it),it=1,4),(ycn(i-1,j+1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
82 FORMAT(1x,'XCN(IM JP=',4f10.7/1x,'YCN(IM JP=',4f10.7)
 
   nct  = icn(i,j,1) + icn(i,j,2) + icn(i,j,3) + icn(i,j,4)
   icls = ixb(i,j,1) + ixb(i,j,2) + ixb(i,j,3) + ixb(i,j,4) + &
          iyb(i,j,1) + iyb(i,j,2) + iyb(i,j,3) + iyb(i,j,4)
 
   WRITE(msg,32) nct,icls,(icn(i,j,it),it=1,4),nfl(i,j) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
32 FORMAT(1x,'NCT ICLS=',2i4/5x,'ICN=',4i4/5x,'NFL=',i4)
 
   WRITE(msg,49) i  ,j,(xfg(i  ,j,it),it=1,4),(yfg(i  ,j,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
   WRITE(msg,50) i+1,j,(xfg(i+1,j,it),it=1,4),(yfg(i+1,j,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
   WRITE(msg,51) i-1,j,(xfg(i-1,j,it),it=1,4),(yfg(i-1,j,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
   WRITE(msg,52) i,j+1,(xfg(i,j+1,it),it=1,4),(yfg(i,j+1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
   WRITE(msg,53) i,j-1,(xfg(i,j-1,it),it=1,4),(yfg(i,j-1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
49 FORMAT(1x,'I  J  =',2i4,' XFG=',4f10.7/16x,' YFG=',4f10.7)
50 FORMAT(1x,'IP J  =',2i4,' XFG=',4f10.7/16x,' YFG=',4f10.7)
51 FORMAT(1x,'IM J  =',2i4,' XFG=',4f10.7/16x,' YFG=',4f10.7)
52 FORMAT(1x,'I  JP =',2i4,' XFG=',4f10.7/16x,' YFG=',4f10.7)
53 FORMAT(1x,'I  JM =',2i4,' XFG=',4f10.7/16x,' YFG=',4f10.7)

   WRITE(msg,54) i+1,j+1,(xfg(i+1,j+1,it),it=1,4),(yfg(i+1,j+1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
   WRITE(msg,55) i+1,j-1,(xfg(i+1,j-1,it),it=1,4),(yfg(i+1,j-1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
   WRITE(msg,56) i-1,j+1,(xfg(i-1,j+1,it),it=1,4),(yfg(i-1,j+1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
   WRITE(msg,57) i-1,j-1,(xfg(i-1,j-1,it),it=1,4),(yfg(i-1,j-1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
54 FORMAT(1x,'IP JP =',2i4,' XFG=',4f10.7/16x,' YFG=',4f10.7)
55 FORMAT(1x,'IP JM =',2i4,' XFG=',4f10.7/16x,' YFG=',4f10.7)
56 FORMAT(1x,'IM JP =',2i4,' XFG=',4f10.7/16x,' YFG=',4f10.7)
57 FORMAT(1x,'IM JM =',2i4,' XFG=',4f10.7/16x,' YFG=',4f10.7)
 
   WRITE(msg,33) (ixb(i,j,it),it=1,4),(iyb(i,j,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
   WRITE(msg,34) (ixb(i+1,j,it),it=1,4),(iyb(i+1,j,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
   WRITE(msg,35) (ixb(i-1,j,it),it=1,4),(iyb(i-1,j,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
   WRITE(msg,36) (ixb(i,j+1,it),it=1,4),(iyb(i,j+1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
   WRITE(msg,37) (ixb(i,j-1,it),it=1,4),(iyb(i,j-1,it),it=1,4) 
   call wrf_debug(_FR_DBG_LEVEL_ ,msg)
33 FORMAT(1x,'IXB(I  J )=',4i4/1x,'IYB(I  J )=',4i4)
34 FORMAT(1x,'IXB(IP J )=',4i4/1x,'IYB(IP J )=',4i4)
35 FORMAT(1x,'IXB(IM J )=',4i4/1x,'IYB(IM J )=',4i4)
36 FORMAT(1x,'IXB(I  JP)=',4i4/1x,'IYB(I  JP)=',4i4)
37 FORMAT(1x,'IXB(I  JM)=',4I4/1x,'IYB(I  JM)=',4i4)

! ---- EGP commented out printing the winds...
!     ... print winds used to spread fire
!  WRITE(msg,180) i  ,j,(ug(i  ,j,it),it=1,4),(vg(i  ,j,it),it=1,4) 
!  call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!  WRITE(msg,181) i+1,j,(ug(i+1,j,it),it=1,4),(vg(i+1,j,it),it=1,4) 
!  call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!  WRITE(msg,182) i-1,j,(ug(i-1,j,it),it=1,4),(vg(i-1,j,it),it=1,4) 
!  call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!  WRITE(msg,183) i,j+1,(ug(i,j+1,it),it=1,4),(vg(i,j+1,it),it=1,4) 
!  call wrf_debug(_FR_DBG_LEVEL_ ,msg)
!  WRITE(msg,184) i,j-1,(ug(i,j-1,it),it=1,4),(vg(i,j-1,it),it=1,4) 
!  call wrf_debug(_FR_DBG_LEVEL_ ,msg)
180 FORMAT(1x,'I  J  =',2i4,' UG=',4f9.3/16x,' VG=',4f9.3)
181 FORMAT(1x,'IP J  =',2i4,' UG=',4f9.3/16x,' VG=',4f9.3)
182 FORMAT(1x,'IM J  =',2i4,' UG=',4f9.3/16x,' VG=',4f9.3)
183 FORMAT(1x,'I  JP =',2i4,' UG=',4f9.3/16x,' VG=',4f9.3)
184 FORMAT(1x,'I  JM =',2i4,' UG=',4f9.3/16x,' VG=',4f9.3)
 
   WRITE (iflg,'(I5.5)') iflt
   WRITE (msg,*) 'fire_error_debug: FATAL '//iflg
   CALL wrf_error_fatal ( msg )

9999 CONTINUE
   RETURN

END SUBROUTINE fire_error_debug

! =========================================================================

REAL FUNCTION cvmgp(a,b,c)

! ----- returns x1 if x3 >=0 
!            or x2 if x3 < 0

    REAL, INTENT(in) :: a,b,c

! ----- begin

    IF (c >= 0.) THEN
       cvmgp = a
    ELSE
       cvmgp = b
    END IF

END FUNCTION cvmgp

! =========================================================================

SUBROUTINE fire_emissions(grnhfx,canhfx,dt,dz8w,rho,num_scalars,  &  ! incoming
                          ids,ide, kds,kde, jds,jde,              &
                          ims,ime, kms,kme, jms,jme,              &
                          its,ite, kts,kte, jts,jte,              &
                          scalar)                                    ! outgoing

! -------------------------------------------------------------------------
!  this routine handles the emission of carbonaceous particles assuming
!  a specified percent of the fuel mass to be emitted as smoke.
!
!  this routine will serve as a template for further chemical emissions.
! -------------------------------------------------------------------------

   IMPLICIT NONE

! --- incoming variables

   INTEGER, INTENT(in) :: ids,ide, kds,kde, jds,jde, &
                          ims,ime, kms,kme, jms,jme, &
                          its,ite, kts,kte, jts,jte

   INTEGER, INTENT(in) :: num_scalars

   REAL, INTENT(in) :: dt
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: grnhfx,canhfx
   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: dz8w
   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: rho

! --- outgoing variables

   REAL, INTENT(out), DIMENSION( ims:ime,jms:jme,1:num_scalars ) :: scalar

! --- local variables

   INTEGER :: i,j,k

   REAL :: smoke_fac
   REAL :: del_g,del_c
   REAL :: density_g,density_c
   REAL :: zbox_g,zbox_c
   REAL :: tracer_g,tracer_c

! ---------------------------------------------------------------------

   smoke_fac = 0.02	! 2% of fuel mass becomes smoke

   DO j = MAX(jts,jds+1),MIN(jte,jde-1)
      DO i = MAX(its,ids+1),MIN(ite,ide-1)

         del_g = grnhfx(i,j) * dt / cmbcnst
         del_c = canhfx(i,j) * dt / cmbcnst

         density_g = rho(i,1,j)
         density_c = rho(i,2,j)

         zbox_g = dz8w(i,1,j)
         zbox_c = dz8w(i,2,j)

         ! --- tracer_g/c in kg of tracer per kg of air

         tracer_g = smoke_fac * del_g / (zbox_g * density_g)
         tracer_c = smoke_fac * del_c / (zbox_c * density_c)

         scalar(i,1,j) = scalar(i,1,j) + tracer_g
         scalar(i,2,j) = scalar(i,2,j) + tracer_c

      END DO
   END DO

END SUBROUTINE fire_emissions

! =========================================================================

SUBROUTINE fire_winds(u,v,                       &  ! incoming
                     ids,ide, kds,kde, jds,jde, &
                     ims,ime, kms,kme, jms,jme, &
                     its,ite, kts,kte, jts,jte, &
                     u_i,v_i)                      ! outgoing

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

! =========================================================================

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

   REAL, INTENT(in) :: alfg	! extinction depth ground fire heat (m)
   REAL, INTENT(in) :: alfc	! extinction depth crown  fire heat (m)
   REAL, INTENT(in) :: z1can	! height of crown fire heat release (m)

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

   cp_i = 1./cp		! inverse of specific heat
   xlv_i = 1./xlv	! inverse of latent heat

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
!	multiply by dry air mass (mu) to eliminate the need to 
!	call sr. calculate_phy_tend (in dyn_em/module_em.F)

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

! =========================================================================

END MODULE module_fr_cawfe
