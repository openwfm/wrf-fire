!
!*** Jan Mandel August-October 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
! This file contains parts copied and/or adapted from earlier codes by 
! Terry Clark, Janice Coen, Don Latham, and Net Patton.

module module_fr_sfire_phys

use module_model_constants, only: cp,xlv
use module_fr_sfire_util

implicit none
PRIVATE

type fire_params
real,pointer,dimension(:,:):: vx,vy                ! wind velocity (m/s)
real,pointer,dimension(:,:):: zsf                  ! terrain height (m)
real,pointer,dimension(:,:):: dzdxf,dzdyf          ! terrain grad (1)
real,pointer,dimension(:,:):: bbb,phisc,phiwc,r_0 ! spread formula coefficients
real,pointer,dimension(:,:):: fgip                 ! init mass of surface fuel (kg/m^2)
real,pointer,dimension(:,:):: ischap               ! 1 if chapparal
real,pointer,dimension(:,:):: fuel_time            ! time to burn to 1/e (s)
real,pointer,dimension(:,:):: fmc_g                ! fuel moisture contents, ground (1)
end type fire_params

! subroutenes and functions 
PUBLIC:: init_fuel_cats,fire_ros,heat_fluxes,set_nfuel_cat,set_fire_params, &
write_fuels_m,fire_risk,fire_intensity,set_fp_from_grid

! types
public:: fire_params

! variables
PUBLIC:: fire_wind_height,fcz0,fcwh,have_fuel_cats,nfuelcats,no_fuel_cat,windrf
! NOTE: fcwh and fcz0 are called fwh and fz0 in read/write statements


! Following table copied from module_fr_cawfe_fuel by Ned Patton with minor changes. 
! Based on:           Clark, T. L., J. L. Coen and D. Latham: 2004, 
!                      "Description of a coupled atmosphere-fire model",
!                      International Journal of Wildland Fire, 13, 49-63.
!
! edited by Jan Mandel   jmandel@ucar.edu  September 2007
!
! - moved all fuel related constants and the initialization subroutine here
! - copied descriptions for fuel categories from fire_sfc.m4 in the original CAWFE code 
! This file had to be copied under a new name because packages in wrf physics
! layer are not allowed to call each other.

!D in col 2 means quantity derived from the others
!
!  Scalar constants (data same for all fuel categories):
!       HFGL           SURFACE FIRE HEAT FLUX THRESHOLD TO IGNITE CANOPY (W/m^2)
!       CMBCNST        JOULES PER KG OF DRY FUEL
!       FUELHEAT       FUEL PARTICLE LOW HEAT CONTENT, BTU/LB
!       FUELMC_G       FUEL PARTICLE (SURFACE) MOISTURE CONTENT
!D      BMST           RATIO OF LATENT TO SENSIBLE HEAT FROM SFC BURN:
!                        % of total fuel mass that is water (not quite
!                        = % fuel moisture).    BMST= (H20)/(H20+DRY)
!                        so BMST = FUELMC_G / (1 + FUELMC_G)  where
!                        FUELMC_G = ground fuel moisture
!
!  Data arrays indexed by fuel category:
!       FGI            INITIAL TOTAL MASS OF SURFACE FUEL (KG/M**2)
!       FUELDEPTHM     FUEL DEPTH, IN M  (CONVERTED TO FT)              
!       SAVR           FUEL PARTICLE SURFACE-AREA-TO-VOLUME RATIO, 1/FT
!                         GRASS: 3500., 10 hr fuel: 109.,  100 hr fuel: 30.
!       FUELMCE        MOISTURE CONTENT OF EXTINCTION; 0.30 FOR MANY DEAD FUELS; 0.15 FOR GRASS
!       FUELDENS       OVENDRY PARTICLE DENSITY, LB/FT^3
!       ST             FUEL PARTICLE TOTAL MINERAL CONTENT
!       SE             FUEL PARTICLE EFFECTIVE MINERAL CONTENT
!       WEIGHT         WEIGHTING PARAMETER THAT DETERMINES THE SLOPE OF THE MASS LOSS CURVE
!                        RANGES FROM ~5 (FAST BURNUP) TO 1000 ( ~40% DECR OVER 10 MIN).
!       FCI_D          INITIAL DRY MASS OF CANOPY FUEL
!       FCT            BURN OUT TIME FOR CANOPY FUEL, AFTER DRY (S)
!       ichap          1 if chaparral, 0 if not
!D      FCI            INITIAL TOTAL MASS OF CANOPY FUEL
!D      FCBR           FUEL CANOPY BURN RATE (KG/M**2/S) 

!

! scalar fuel coefficients
   REAL, SAVE:: cmbcnst,hfgl,fuelmc_g,fuelmc_c, fire_wind_height
! computed values
   REAL, SAVE:: fuelheat

! defaults, may be changed in init_fuel_cats
   DATA cmbcnst  / 17.433e+06/             ! J/kg dry fuel
   DATA hfgl     / 17.e4 /                ! W/m^2
   DATA fuelmc_g / 0.08  /                ! set = 0 for dry ground fuel
   DATA fuelmc_c / 1.00  /                ! set = 0 for dry canopy
   DATA fire_wind_height/6.096/           ! m, 6.096m Behave, 0 to use fwh in each category
!  REAL, PARAMETER :: bmst     = fuelmc_g/(1+fuelmc_g)
!  REAL, PARAMETER :: fuelheat = cmbcnst * 4.30e-04     ! convert J/kg to BTU/lb
!  real, parameter :: xlv      = 2.5e6                  ! to make it selfcontained
!  real, parameter :: cp      =  7.*287./2              ! to make it selfcontained


! fuel categorytables
   INTEGER, PARAMETER :: nf=14              ! fuel cats in data stmts, for fillers only`
   INTEGER, SAVE      :: nfuelcats = 13     ! number of fuel categories, can be reset from namelist.fire
   INTEGER, PARAMETER :: mfuelcats = 30     ! number of fuel categories 
   INTEGER, PARAMETER :: zf = mfuelcats-nf  ! number of zero fillers in data stmt 
   INTEGER, SAVE      :: no_fuel_cat = 14   ! special category outside of 1:nfuelcats
   INTEGER, SAVE      :: ibeh=1             ! type of spread formula
   CHARACTER (len=80), DIMENSION(mfuelcats ), save :: fuel_name
   INTEGER, DIMENSION( mfuelcats ), save :: ichap
   REAL   , DIMENSION( mfuelcats ), save :: windrf,weight,fgi,fci,fci_d,fct,fcbr, &
                                            fueldepthm,fueldens,fuelmce,   &
                                            fcwh,fcz0, ffw, &
                                            savr,st,se,adjr0,adjrw,adjrs
! =============================================================================
! Standard 13 fire behavior fuel models (for surface fires), along with some
!          estimated canopy properties (for crown fire).
! =============================================================================
   DATA fuel_name /  &
     'FUEL MODEL 1: Short grass (1 ft)', &
     'FUEL MODEL 2: Timber (grass and understory)', &
     'FUEL MODEL 3: Tall grass (2.5 ft)', &
     'FUEL MODEL 4: Chaparral (6 ft)', &
     'FUEL MODEL 5: Brush (2 ft) ', &
     'FUEL MODEL 6: Dormant brush, hardwood slash', &
     'FUEL MODEL 7: Southern rough', &
     'FUEL MODEL 8: Closed timber litter', &
     'FUEL MODEL 9: Hardwood litter', &
     'FUEL MODEL 10: Timber (litter + understory)', &
     'FUEL MODEL 11: Light logging slash', &
     'FUEL MODEL 12: Medium logging slash', &
     'FUEL MODEL 13: Heavy logging slash', &
     'FUEL MODEL 14: no fuel', &
     zf*' '/
   DATA windrf /0.36, 0.36, 0.44,  0.55,  0.42,  0.44,  0.44, &     
                0.36, 0.36, 0.36,  0.36,  0.43,  0.46,  1e-7, zf*0 / ! added jmandel October 2010
   DATA fgi / 0.166, 0.897, 0.675, 2.468, 0.785, 1.345, 1.092, &
              1.121, 0.780, 2.694, 2.582, 7.749, 13.024, 1.e-7, zf*0.  /
   DATA fueldepthm /0.305,  0.305,  0.762, 1.829, 0.61,  0.762,0.762, &
                    0.0610, 0.0610, 0.305, 0.305, 0.701, 0.914, 0.305,zf*0. /
   DATA savr / 3500., 2784., 1500., 1739., 1683., 1564., 1562.,  &
               1889., 2484., 1764., 1182., 1145., 1159., 3500., zf*0. /
   DATA fuelmce / 0.12, 0.15, 0.25, 0.20, 0.20, 0.25, 0.40,  &
                  0.30, 0.25, 0.25, 0.15, 0.20, 0.25, 0.12 , zf*0. / 
   DATA fueldens / nf * 32., zf*0. /   ! 32 if solid, 19 if rotten.
   DATA st / nf* 0.0555 , zf*0./
   DATA se / nf* 0.010 , zf*0./
! ----- Notes on weight: (4) - best fit of Latham data;
!                 (5)-(7) could be 60-120; (8)-(10) could be 300-1600;
!                 (11)-(13) could be 300-1600
   DATA weight / 7.,  7.,  7., 180., 100., 100., 100.,  &
              900., 900., 900., 900., 900., 900., 7. , zf*0./ 
! ----- 1.12083 is 5 tons/acre.  5-50 tons/acre orig., 100-300 after blowdown
   DATA fci_d / 0., 0., 0., 1.123, 0., 0., 0.,  &
            1.121, 1.121, 1.121, 1.121, 1.121, 1.121, 0., zf*0./
   DATA fct / 60., 60., 60., 60., 60., 60., 60.,  &
            60., 120., 180., 180., 180., 180. , 60. , zf*0.   /
   DATA ichap / 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 , zf*0/ 
   DATA fcwh  / 6.096, 6.096, 6.096, 6.096, 6.096, 6.096, 6.096, &
         6.096, 6.096, 6.096, 6.096, 6.096, 6.096, 6.096, zf*0. / ! consistent with BEHAVE
! roughness length 0.13*fueldepthm except cat 3 fz0=0.1 for consistency with landuse
 ! fz0 =   0.0396,0.0396,0.0991,0.2378,0.0793,0.0991,0.0991,
   DATA fcz0 /  0.0396,0.0396,0.1000,0.2378,0.0793,0.0991,0.0991, &
         0.0079,0.0079,0.0396,0.0396,0.0911,0.1188,0.0396, zf * 0. /
   !DATA fcwh /0.6 , 0.6,  1.5, 36,  1.2,  1.5, 1.5, &
   !           0.12, 0.12, 0.6, 0.6, 1.38, 1.8, 1.8, zf*0 / ! fuel wind height, added jm 2/23/11
   !DATA fcz0 /0.3, 0.3, 0.75, 18.,  0.6,  0.75, 0.75, &
   !           0.06, 0.06, 0.3, 0.3, 0.69, 0.9, 0.9, zf*0 /  ! fuel roughness height, added jm 2/23/11
   DATA ffw /nf* 0.9, zf*0/
   DATA adjr0 /mfuelcats*1./
   DATA adjrw /mfuelcats*1./
   DATA adjrs /mfuelcats*1./

! moisture behavior
  integer, parameter::max_moisture_classes=5
  integer, parameter::zm=max_moisture_classes - 3
  integer:: moisture_classes=3
  real, dimension(max_moisture_classes):: drying_lag,wetting_lag,saturation_moisture,hysteresis,saturation_rain 
  integer, dimension(max_moisture_classes):: drying_model,wetting_model
    
  real, dimension(7)::eq_p

  data drying_lag                 /1., 10., 100. , zm*0./  ! time lag (h) approaching equilibrium moisture 
  data hysteresis                 /0., 0., 0.    , zm*0./  ! equilibrium moisture increases by 1/2 of this
                                                  ! when approached from above and decreases when from below
  data wetting_lag                   /14, 140., 1400., zm*0./  ! time lag (h) for approaching saturation in rain
                                                  ! 7% / hour initial rise per Fosberg and Deeming (1971)
  data saturation_moisture        /2.5, 2.5, 2.5 , zm*0./  ! saturation moisture contents (1) in rain
  data saturation_rain            /8.0, 8.0, 8.0 , zm*0./  ! stronger rain matters only in duration (mm/h) 
  data drying_model               /1,   1,   1,    zm*0 /
  data wetting_model              /1,   1,   1,    zm*0 /
  data eq_p/  1.035e-09, &   !(3.893e-10, 1.681e-09) ! coefficients of the equilibrium fuel moisture polynomial
              -2.62e-07, &   !(-4.593e-07, -6.473e-08) ! fitted from the graph in Schroeder and Buck (1970)
              2.507e-05, &   !(2.194e-06, 4.795e-05)
              -0.001107, &   !(-0.002353, 0.000139)
                0.02245, &   !(-0.009188, 0.05409)
               -0.05901, &   !(-0.3721, 0.254)
                  3.043/   !(2.17, 3.915)

! =========================================================================

  logical, save :: have_fuel_cats=.false.

contains

subroutine advance_moisture(    &
    initialize,                 & ! 0 = run,  1 = initialize previous values, 2 = initialize also the moisture
    fmc_gc_init,                & ! initial value of moisture
    ims,ime,  jms,jme,          & ! memory dimensions
    its,ite,  jts,jte,          & ! tile dimensions
    nfmc,                       & ! number of moisture fields
    timestep,                   & ! moisture model time step
    rainc, rainnc,              & ! accumulated rain 
    t2, q2, psfc,               & ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
    rain_old,                   & ! previous value of accumulated rain
    t2_old, q2_old, psfc_old,   & ! previous values of the atmospheric state at surface
    rh_sfc,                     & ! relative humidity
    fmc_gc                      & ! fuel moisture fields updated, by class, assumed set to something reasonable
    )

!*** arguments
integer, intent(in)::           &
    initialize,                 & ! 0 = run,  other = initialize
    ims,ime,  jms,jme,          & ! memory dimensions
    its,ite,  jts,jte,          & ! tile dimensions
    nfmc                          ! number of moisture fields
real, intent(in):: fmc_gc_init,timestep
real, intent(in), dimension(ims:ime,jms:jme):: t2, q2, psfc, rainc, rainnc
real, intent(inout), dimension(ims:ime,jms:jme):: t2_old, q2_old, psfc_old, rain_old 
real, intent(out), dimension(ims:ime,jms:jme):: rh_sfc ! diagnostics
real, intent(inout), dimension(ims:ime,jms:jme,nfmc):: fmc_gc

!*** local
integer:: i,j,k
real::rain_int, T, P, Q, QRS, ES, RH, tend, EMC_d, EMC_w, EMC, R
real::rain_thr=0.05

!*** executable

if(initialize.ge.1)call copy2old
if(initialize.ge.2)call init_fmc_gc

do j=jts,jte
    do i=its,ite
       rain_int = ((rainc(i,j) + rainnc(i,j)) - rain_old(i,j))/timestep ! rain intensity
       R = rain_int - rain_thr
       do k=1,nfmc
          if (R > 0.) then
              select case(wetting_model(k))
                  case(1) ! saturation_moisture=2.5 wetting_lag=14h saturation_rain=8 mm/h calibrated to VanWagner&Pickett 1985 per 24 hours
                      tend=(saturation_moisture(k)-fmc_gc(i,j,k))/wetting_lag(k) * (1. - exp(R/saturation_rain(k)))
              end select
          else ! not raining
              ! relative humidity
              T = t2(i,j)
              P = psfc(i,j)
              Q = q2(i,j)
              ES=610.78*exp(17.269*(T-273.161)/(T-35.861));
              QRS=0.622*ES/(P-0.378*ES);
              RH = 100.*Q/QRS;
              rh_sfc(i,j)=RH ! diagnostics
              select case(drying_model(k))
                  case(1) ! Van Wagner formula (1972) per Viney (1991)
                      EMC_d=0.924*RH**0.679 + 0.000499*exp(0.1*RH) + 0.18*(21.1-T)*(1-exp(-0.115*RH)) ! equilibrium moisture for drying
                      EMC_w=0.618*RH**0.753 + 0.000454*exp(0.1*RH) + 0.18*(21.1-T)*(1-exp(-0.115*RH)) ! equilibrium moisture for adsorbtion
                      if (fmc_gc(i,j,k) > EMC_d) then
                          tend = (EMC_d - fmc_gc(i,j,k)) / drying_lag(k)
                      elseif(fmc_gc(i,j,k) < EMC_w)then 
                          tend = (EMC_w - fmc_gc(i,j,k)) / drying_lag(k)
                      else
                          tend = 0.
                      endif
              end select
          endif
          fmc_gc(i,j,k) = fmc_gc (i,j,k)+ timestep*tend
      enddo
   enddo
enddo
          
call copy2old
return

contains

subroutine copy2old

do j=jts,jte
    do i=its,ite
        rain_old(i,j) = rainc(i,j) + rainnc(i,j)
        t2_old(i,j) = t2(i,j)
        q2_old(i,j) = q2(i,j)
        psfc_old(i,j) = psfc(i,j)
    enddo
enddo

end subroutine copy2old

subroutine init_fmc_gc
    do k=1,nfmc
        do j=jts,jte
            do i=its,ite
                fmc_gc(i,j,k)=0.1
            enddo
        enddo
    enddo
end subroutine init_fmc_gc


end subroutine advance_moisture


subroutine set_fp_from_grid(grid,fp)
    implicit none
    type(domain),intent(in)::grid
    type(fire_params),intent(out)::fp

    ! pointers to be passed to fire spread formulas
    fp%vx => grid%uf         ! fire winds
    fp%vy => grid%vf         ! fire winds
    fp%zsf => grid%zsf       ! terrain height
    fp%dzdxf => grid%dzdxf   ! terrain grad
    fp%dzdyf => grid%dzdyf   ! terrain grad
    fp%bbb => grid%bbb       ! spread formula coeff
    fp%phisc => grid%phisc   ! spread formula coeff
    fp%phiwc => grid%phiwc   ! spread formula coeff
    fp%r_0 => grid%r_0       ! spread formula coeff
    fp%fgip => grid%fgip     ! spread formula coeff
    fp%ischap => grid%ischap ! spread formula coeff
    fp%fuel_time => grid%fuel_time ! time for fuel to burn to 1/e
    fp%fmc_g => grid%fmc_g   ! fuel moisture, ground

end subroutine set_fp_from_grid


subroutine init_fuel_cats
implicit none
!*** purpose: initialize fuel tables and variables by constants
!*** arguments: none
logical, external:: wrf_dm_on_monitor
!$ integer, external:: OMP_GET_THREAD_NUM
!*** local
integer:: i,j,k,ii,iounit
character(len=128):: msg
REAL   , DIMENSION( mfuelcats ) :: fwh, fz0
!*** executable

! read 
namelist /fuel_scalars/ cmbcnst,hfgl,fuelmc_g,fuelmc_c,nfuelcats,no_fuel_cat,fire_wind_height,ibeh
namelist /fuel_categories/ fuel_name,windrf,fgi,fueldepthm,savr, &
    fuelmce,fueldens,st,se,weight,fci_d,fct,ichap,fwh,fz0,ffw,adjr0,adjrw,adjrs

!$  if (OMP_GET_THREAD_NUM() .ne. 0)then
!$     call crash('init_fuel_cats: must be called from master thread')
!$  endif

IF ( wrf_dm_on_monitor() ) THEN
    ! we are the master task
    ! copy in defaults
    fwh=fcwh
    fz0=fcz0
    ! read the file
    iounit=open_text_file('namelist.fire','read')
    read(iounit,fuel_scalars)
    read(iounit,fuel_categories)
    CLOSE(iounit)
    ! copy out to permanent names
    fcwh=fwh
    fcz0=fz0
    
    if (nfuelcats>mfuelcats) then
        write(msg,*)'nfuelcats=',nfuelcats,' too large, increase mfuelcats'
        call crash(msg)
    endif
        if (no_fuel_cat >= 1 .and. no_fuel_cat <= nfuelcats)then
        write(msg,*)'no_fuel_cat=',no_fuel_cat,' may not be between 1 and nfuelcats=',nfuelcats
        call crash(msg)
    endif
ENDIF

! broadcast the contents of the file
call wrf_dm_bcast_real(cmbcnst,1)
call wrf_dm_bcast_real(hfgl,1)
call wrf_dm_bcast_real(fuelmc_g,1)
call wrf_dm_bcast_real(fuelmc_c,1)
call wrf_dm_bcast_real(fire_wind_height,1)
call wrf_dm_bcast_integer(nfuelcats,1)
call wrf_dm_bcast_integer(no_fuel_cat,1)
call wrf_dm_bcast_integer(ibeh,1)
call wrf_dm_bcast_real(windrf,    nfuelcats)
call wrf_dm_bcast_real(fgi,       nfuelcats)
call wrf_dm_bcast_real(fueldepthm,nfuelcats)
call wrf_dm_bcast_real(savr,      nfuelcats)
call wrf_dm_bcast_real(fuelmce,   nfuelcats)
call wrf_dm_bcast_real(fueldens,  nfuelcats)
call wrf_dm_bcast_real(st,        nfuelcats)
call wrf_dm_bcast_real(se,        nfuelcats)
call wrf_dm_bcast_real(weight,    nfuelcats)
call wrf_dm_bcast_real(fci_d,     nfuelcats)
call wrf_dm_bcast_real(fct,       nfuelcats)
call wrf_dm_bcast_integer(ichap,  nfuelcats)
call wrf_dm_bcast_real(fcwh,      nfuelcats)
call wrf_dm_bcast_real(fcz0,      nfuelcats)
call wrf_dm_bcast_real(ffw,       nfuelcats)
call wrf_dm_bcast_real(adjr0,     nfuelcats)
call wrf_dm_bcast_real(adjrw,     nfuelcats)
call wrf_dm_bcast_real(adjrs,     nfuelcats)

! compute derived scalars

fuelheat = cmbcnst * 4.30e-04     ! convert J/kg to BTU/lb

! compute derived fuel category coefficients 

DO i = 1,nfuelcats
    fci(i) = (1.+fuelmc_c)*fci_d(i)
    if(fct(i) .ne.  0.)then
        fcbr(i) = fci_d(i)/fct(i) ! to avoid division by zero
    else
        fcbr(i) = 0
    endif
END DO

! prints

call message('**********************************************************')
call message('FUEL COEFFICIENTS')
write(msg,8)'cmbcnst    ',cmbcnst
call message(msg)
write(msg,8)'hfgl       ',hfgl
call message(msg)
write(msg,8)'fuelmc_g   ',fuelmc_g
call message(msg)
write(msg,8)'fuelmc_c   ',fuelmc_c
call message(msg)
write(msg,8)'fuelheat   ',fuelheat
call message(msg)
write(msg,7)'nfuelcats  ',nfuelcats
call message(msg)
write(msg,7)'no_fuel_cat',no_fuel_cat
call message(msg)

j=1
7 format(a,5(1x,i8,4x))
8 format(a,5(1x,g12.5e2))
9 format(a,5(1x,a))
do i=1,nfuelcats,j
    k=min(i+j-1,nfuelcats)
    call message(' ')
    write(msg,7)'CATEGORY  ',(ii,ii=i,k)
    call message(msg)
    write(msg,9)'fuel name ',(fuel_name(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fwh       ',(fcwh(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fz0       ',(fcz0(ii),ii=i,k)
    call message(msg)
    write(msg,8)'windrf    ',(windrf(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fgi       ',(fgi(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fueldepthm',(fueldepthm(ii),ii=i,k)
    call message(msg)
    write(msg,8)'savr      ',(savr(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fuelmce   ',(fuelmce(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fueldens  ',(fueldens(ii),ii=i,k)
    call message(msg)
    write(msg,8)'st        ',(st(ii),ii=i,k)
    call message(msg)
    write(msg,8)'se        ',(se(ii),ii=i,k)
    call message(msg)
    write(msg,8)'weight    ',(weight(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fci_d     ',(fci_d(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fct       ',(fct(ii),ii=i,k)
    call message(msg)
    write(msg,7)'ichap     ',(ichap(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fci       ',(fci(ii),ii=i,k)
    call message(msg)
    write(msg,8)'fcbr      ',(fcbr(ii),ii=i,k)
    call message(msg)
    write(msg,8)'ffw       ',(ffw(ii),ii=i,k)
    call message(msg)
    write(msg,8)'adjr0     ',(adjr0(ii),ii=i,k)
    call message(msg)
    write(msg,8)'adjrw     ',(adjrw(ii),ii=i,k)
    call message(msg)
    write(msg,8)'adjrs     ',(adjrs(ii),ii=i,k)
    call message(msg)
enddo
call message('**********************************************************')

have_fuel_cats=.true.

! and print to file
IF ( wrf_dm_on_monitor() ) THEN
  call write_fuels_m(61,30.,1.)
ENDIF


end subroutine init_fuel_cats


subroutine write_fuels_m(nsteps,maxwind,maxslope)
implicit none
integer, intent(in):: nsteps   ! number of steps for speed computation
real, intent(in):: maxwind,maxslope ! computer from zero to these

integer:: iounit,k,j,i
type(fire_params)::fp
real, dimension(1:2,1:nsteps), target::vx,vy,zsf,dzdxf,dzdyf,bbb,phisc,phiwc,r_0,fgip,ischap,fmc_g
real, dimension(1:2,1:nsteps)::nfuel_cat,fuel_time,ros,fwh,fz0
real::ros_back,ros_wind,ros_slope,propx,propy,r

if(.not.have_fuel_cats)call crash('write_fuels_m: fuel categories not yet set')

fp%vx=>vx
fp%vy=>vy
fp%dzdxf=>dzdxf
fp%dzdyf=>dzdyf
fp%bbb=>bbb
fp%phisc=>phisc
fp%phiwc=>phiwc
fp%r_0=>r_0
fp%fgip=>fgip
fp%ischap=>ischap
fp%fmc_g=>fmc_g

iounit = open_text_file('fuels.m','write')

10 format('fuel(',i3,').',a,'=',"'",a,"'",';% ',a)
do k=1,nfuelcats
    write(iounit,10)k,'fuel_name',trim(fuel_name(k)),'FUEL MODEL NAME'
    call write_var(k,'windrf',windrf(k),'WIND REDUCTION FACTOR FROM FCWH TO MIDFLAME HEIGHT' )
    call write_var(k,'fwh',fcwh(k),'WIND HEIGHT TO INTERPOLATE VERTICALLY TO (M)' )
    call write_var(k,'fz0',fcz0(k),'ROUGHNESS LENGTH FOR VERTICAL WIND LOG INTERPOLATION (M)' )
    call write_var(k,'fgi',fgi(k),'INITIAL TOTAL MASS OF SURFACE FUEL (KG/M**2)' )
    call write_var(k,'fueldepthm',fueldepthm(k),'FUEL DEPTH (M)')
    call write_var(k,'savr',savr(k),'FUEL PARTICLE SURFACE-AREA-TO-VOLUME RATIO, 1/FT')
    call write_var(k,'fuelmce',fuelmce(k),'MOISTURE CONTENT OF EXTINCTION')
    call write_var(k,'fueldens',fueldens(k),'OVENDRY PARTICLE DENSITY, LB/FT^3')
    call write_var(k,'st',st(k),'FUEL PARTICLE TOTAL MINERAL CONTENT')
    call write_var(k,'se',se(k),'FUEL PARTICLE EFFECTIVE MINERAL CONTENT')
    call write_var(k,'weight',weight(k),'WEIGHTING PARAMETER THAT DETERMINES THE SLOPE OF THE MASS LOSS CURVE')
    call write_var(k,'fci_d',fci_d(k),'INITIAL DRY MASS OF CANOPY FUEL')
    call write_var(k,'fct',fct(k),'BURN OUT TIME FOR CANOPY FUEL, AFTER DRY (S)')
    call write_var(k,'ichap',float(ichap(k)),'1 if chaparral, 0 if not')
    call write_var(k,'fci',fci(k),'INITIAL TOTAL MASS OF CANOPY FUEL')
    call write_var(k,'fcbr',fcbr(k),'FUEL CANOPY BURN RATE (KG/M**2/S)')
    call write_var(k,'adjr0',adjr0(k),'MULTIPLICATIVE ADJUSTMENT OF BACKING SPREAD RATE')
    call write_var(k,'adjrw',adjrw(k),'MULTIPLICATIVE ADJUSTMENT OF WIND CONTRIBUTION TO SPREAD RATE')
    call write_var(k,'adjrs',adjrs(k),'MULTIPLICATIVE ADJUSTMENT OF SLOPE CONTRIBUTION TO SPREAD RATE')
    call write_var(k,'ffw',ffw(k),'FUEL FRACTION CONSUMED IN THE FLAMING ZONE')
    call write_var(k,'hfgl',hfgl,'SURFACE FIRE HEAT FLUX THRESHOLD TO IGNITE CANOPY (W/m^2)')
    call write_var(k,'cmbcnst',cmbcnst,'JOULES PER KG OF DRY FUEL')
    call write_var(k,'fuelheat',fuelheat,'FUEL PARTICLE LOW HEAT CONTENT, BTU/LB')
    call write_var(k,'fuelmc_g',fuelmc_g,'FUEL PARTICLE (SURFACE) MOISTURE CONTENT')
    call write_var(k,'fuelmc_c',fuelmc_c,'FUEL PARTICLE (CANOPY) MOISTURE CONTENT')
    ! set up fuel arrays 
    !subroutine set_fire_params( &
    !                       ifds,ifde,jfds,jfde, &
    !                       ifms,ifme,jfms,jfme, &
    !                       ifts,ifte,jfts,jfte, &
    !                       fdx,fdy,nfuel_cat0,  &
    !                       nfuel_cat,fuel_time, &
    !                       fp ) 
    nfuel_cat = k
    call set_fire_params( &
                           1,2,1,nsteps, &
                           1,2,1,nsteps, &
                           1,2,1,nsteps, &
                           0.,0.,k,  &
                           nfuel_cat,fuel_time, &
                           fp ) 
    ! set up windspeed and slope table
    propx=1.
    propy=0.
    do j=1,nsteps
       r=float(j-1)/float(nsteps-1)
       ! line 1 varies windspeed (in x direction), zero slope
       vx(1,j)=maxwind*r
       vy(1,j)=0.
       dzdxf(1,j)=0.
       dzdyf(1,j)=0.
       ! line 2 varies slope (in x direction), zero slope
       vx(2,j)=0.
       vy(2,j)=0.
       dzdxf(2,j)=maxslope*r
       dzdyf(2,j)=0.
    enddo
    do j=1,nsteps
       do i=1,2
          call fire_ros(ros_back,ros_wind,ros_slope, &
             propx,propy,i,j,fp)
          ros(i,j)=ros_back+ros_wind+ros_slope
       enddo
       write(iounit,13)k,'wind',j,vx(1,j),'wind speed'
       write(iounit,13)k,'ros_wind',j,ros(1,j),'rate of spread for the wind speed'
       write(iounit,13)k,'slope',j,dzdxf(2,j),'slope'
       write(iounit,13)k,'ros_slope',j,ros(2,j),'rate of spread for the slope'
    enddo
enddo
13 format('fuel(',i3,').',a,'(',i3,')=',g12.5e2,';% ',a)
 
close(iounit)
! stop

contains

subroutine write_var(k,name,value,descr)
! write entry for one variable
integer, intent(in)::k
character(len=*), intent(in)::name,descr
real, intent(in)::value
write(iounit,11)k,name,value
write(iounit,12)k,name,descr
11 format('fuel(',i3,').',a,'=',g12.5e2,  ';')
12 format('fuel(',i3,').',a,"_descr='",a,"';")
end subroutine write_var

end subroutine write_fuels_m

!
!*******************
!

subroutine set_fire_params( &
                           ifds,ifde,jfds,jfde, &
                           ifms,ifme,jfms,jfme, &
                           ifts,ifte,jfts,jfte, &
                           fdx,fdy,nfuel_cat0,  &
                           nfuel_cat,fuel_time, &
                           fp ) 

implicit none

!*** purpose: Set all fire model params arrays, constant values.

!*** arguments
integer, intent(in)::ifds,ifde,jfds,jfde                        ! fire domain bounds
integer, intent(in)::ifts,ifte,jfts,jfte                        ! fire tile bounds
integer, intent(in)::ifms,ifme,jfms,jfme                        ! memory array bounds
real, intent(in):: fdx,fdy                                      ! fire mesh spacing
integer,intent(in)::nfuel_cat0                                  ! default fuel category, if nfuel_cat=0
real, intent(in),dimension(ifms:ifme, jfms:jfme)::nfuel_cat  ! fuel data
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time   ! fire params arrays
type(fire_params),intent(inout)::fp

!*** local

real::  fuelload, fueldepth, rtemp1, rtemp2, &
        qig, epsilon, rhob, wn, betaop, e, c, &
        xifr, etas, etam, a, gammax, gamma, ratio, ir, &
        fuelloadm,fdxinv,fdyinv,betafl, bmst
integer:: i,j,k
integer::nerr
character(len=128)::msg

!*** executable

if(.not.have_fuel_cats)call crash('set_fire_params: fuel categories not yet set')

nerr=0
do j=jfts,jfte
   do i=ifts,ifte
     ! fuel category 
     k=int( nfuel_cat(i,j) )
     if(k.eq.no_fuel_cat)then   ! no fuel 
        fp%fgip(i,j)=0.            ! no mass 
        fp%ischap(i,j)=0.
        fp%phisc(i,j)=0.          ! 
        fp%bbb(i,j)=1.             !
        fuel_time(i,j)=7./0.85  ! does not matter, just what was there before
        fp%phiwc(i,j)=0.
        fp%r_0(i,j)=0.             ! no fuel, no spread.
     else
        if(k.eq.0.and.nfuel_cat0.ge.1.and.nfuel_cat0.le.nfuelcats)then
            ! replace k=0 by default
            k=nfuel_cat0
            nerr=nerr+1
        endif
   
        if(k.lt.1.or.k.gt.nfuelcats)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
            write(msg,'(3(a,i5))')'nfuel_cat(', i ,',',j,')=',k
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
            call message(msg)
            call crash('set_fire_params: fuel category out of bounds')
        endif

        fuel_time(i,j)=weight(k)/0.85 ! cell based
        
        ! do not understand calculations of stime in binit.m4
        ! set fuel time constant: weight=1000=>40% decrease over 10 min
        ! fuel decreases as exp(-t/fuel_time) 
        ! exp(-600*0.85/1000) = approx 0.6 

        fp%ischap(i,j)=ichap(k)
        fp%fgip(i,j)=fgi(k)
        if(fire_fmc_read.eq.1)then
           fp%fmc_g(i,j)=fuelmc_g
        endif

        ! end jm addition

        !
        !*** rest copied from wf2_janice/fire_startup.m4 with minimal changes
        !

        !     ...Settings of fire spread parameters from Rothermel follows. These
        !        don't need to be recalculated later.
        
        bmst     = fp%fmc_g(i,j) / (1.+fp%fmc_g(i,j))
        fuelloadm= (1.-bmst) * fgi(k)  !  fuelload without moisture
        fuelload = fuelloadm * (.3048)**2 * 2.205    ! to lb/ft^2
        fueldepth = fueldepthm(k)/0.3048               ! to ft
        betafl = fuelload/(fueldepth * fueldens(k))! packing ratio
        betaop = 3.348 * savr(k)**(-0.8189)     ! optimum packing ratio
        qig = 250. + 1116.*fp%fmc_g(i,j)            ! heat of preignition, btu/lb
        epsilon = exp(-138./savr(k) )    ! effective heating number
        rhob = fuelload/fueldepth    ! ovendry bulk density, lb/ft^3

        c = 7.47 * exp( -0.133 * savr(k)**0.55)    ! const in wind coef
        fp%bbb(i,j) = 0.02526 * savr(k)**0.54      ! const in wind coef
        !if(fire_wind_log_interp .eq. 4 .or. fire_use_windrf .eq. 1) then
        !     c = c * windrf(k)**fp%bbb(i,j)             ! jm: multiply wind by reduction factor
        !endif
        e = 0.715 * exp( -3.59e-4 * savr(k))       ! const in wind coef
        fp%phiwc(i,j) = c * (betafl/betaop)**(-e)

        ! phis = 5.275 *(fp%betafl(i,j))**(-0.3) *tanphim**2   ! slope factor
        fp%phisc(i,j) = 5.275 *(betafl)**(-0.3)    ! const in slope coeff

        rtemp2 = savr(k)**1.5
        gammax = rtemp2/(495. + 0.0594*rtemp2)              ! maximum rxn vel, 1/min
        a = 1./(4.774 * savr(k)**0.1 - 7.27)   ! coef for optimum rxn vel
        ratio = betafl/betaop
        gamma = gammax *(ratio**a) *exp(a*(1.-ratio)) !optimum rxn vel, 1/min

        wn = fuelload/(1 + st(k))       ! net fuel loading, lb/ft^2
        rtemp1 = fp%fmc_g(i,j)/fuelmce(k)
        etam = 1.-2.59*rtemp1 +5.11*rtemp1**2 -3.52*rtemp1**3  !moist damp coef
        etas = 0.174* se(k)**(-0.19)                ! mineral damping coef
        ir = gamma * wn * fuelheat * etam * etas  !rxn intensity,btu/ft^2 min
        ! jm irm = ir * 1055./( 0.3048**2 * 60.) * 1.e-6     !for mw/m^2
        ! jm: irm set but never used??

        xifr = exp( (0.792 + 0.681*savr(k)**0.5) &
            * (betafl+0.1)) /(192. + 0.2595*savr(k)) ! propagating flux ratio

!        ... r_0 is the spread rate for a fire on flat ground with no wind.

        fp%r_0(i,j) = ir*xifr/(rhob * epsilon *qig)    ! default spread rate in ft/min
        fp%r_0(i,j) = fp%r_0(i,j) * .00508             ! convert to m/s
        fp%phiwc(i,j) = fp%phiwc(i,j) * fp%r_0(i,j)              ! premultiply wind coefficient so it can be used additively
        fp%phisc(i,j) = fp%phisc(i,j) * fp%r_0(i,j)              ! premultiply wind coefficient so it can be used additively

        ! apply adjustments
        fp%r_0(i,j)   = fp%r_0(i,j)   * adjr0(k)
        fp%phiwc(i,j) = fp%phiwc(i,j) * adjrw(k)
        fp%phisc(i,j) = fp%phisc(i,j) * adjrs(k)
     endif
  enddo
enddo

if(nerr.gt.1)then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
    write(msg,'(a,i6,a)')'set_fire_params: WARNING: fuel category 0 replaced in',nerr,' cells'
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
    call message(msg)
endif

end subroutine set_fire_params

!
!*******************
!

subroutine heat_fluxes(dt,fp,                     &
        ifms,ifme,jfms,jfme,                      &  ! memory dims
        ifts,ifte,jfts,jfte,                      &  ! tile dims
        iffs,iffe,jffs,jffe,                      &  ! fuel_frac_burnt dims
        fgip,fuel_frac_burnt,                     & !in
        grnhft,grnqft)                              !out
implicit none

!*** purpose        
! compute the heat fluxes on the fire grid cells

!*** arguments
type(fire_params), intent(in)::fp
real, intent(in)::dt          ! dt  the fire time step (the fire model advances time by this)
integer, intent(in)::ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,iffs,iffe,jffs,jffe   ! dimensions                   
real, intent(in),dimension(ifms:ifme,jfms:jfme):: fgip
real, intent(in),dimension(iffs:iffe,jffs:jffe):: fuel_frac_burnt
real, intent(out),dimension(ifms:ifme,jfms:jfme):: grnhft
real, intent(out),dimension(ifms:ifme,jfms:jfme),optional:: grnqft

!*** local
integer::i,j
real:: dmass,bmst
logical::latent


!*** executable        
latent = present(grnqft)
do j=jfts,jfte
    do i=ifts,ifte
         dmass =                     &     ! ground fuel dry mass burnt this call (kg/m^2)
             fgip(i,j)               &     ! init mass from fuel model no (kg/m^2) = fgi(nfuel_cat(i,j)
             * fuel_frac_burnt(i,j)        ! fraction burned this call    (1)
         bmst     = fp%fmc_g(i,j)/(1.+fp%fmc_g(i,j))
         grnhft(i,j) = (dmass/dt)*(1.-bmst)*cmbcnst         ! J/m^2/sec
         if(latent)grnqft(i,j) = (bmst+(1.-bmst)*.56)*(dmass/dt)*xlv  ! 
         ! bmst = relative water contents; 0.56 = est. ratio of water from burning
         ! xlv = nominal specific latent heat of water J/kg (dependence on temperature ignored)
         ! xlv is  defined in module_model_constants 
    enddo
enddo

end subroutine heat_fluxes

!
!**********************
!            


subroutine set_nfuel_cat(   &
    ifms,ifme,jfms,jfme,               &
    ifts,ifte,jfts,jfte,               &
    ifuelread,nfuel_cat0,zsf,nfuel_cat)

implicit none

! set fuel distributions for testing
integer, intent(in)::   ifts,ifte,jfts,jfte,               &
                        ifms,ifme,jfms,jfme               

integer, intent(in)::ifuelread,nfuel_cat0
real, intent(in), dimension(ifms:ifme, jfms:jfme)::zsf
real, intent(out), dimension(ifms:ifme, jfms:jfme)::nfuel_cat

!*** local

! parameters to control execution
integer:: i,j,iu1
real:: t1
character(len=128)msg

!$OMP CRITICAL(SFIRE_PHYS_CRIT)
    write(msg,'(a,i3)')'set_nfuel_cat: ifuelread=',ifuelread 
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
    call message(msg)

if (ifuelread .eq. -1 .or. ifuelread .eq. 2) then
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
    call message('set_nfuel_cat: assuming nfuel_cat initialized already') 
    call message(msg)
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
else if (ifuelread .eq. 0) then
!
    do j=jfts,jfte
        do  i=ifts,ifte
            nfuel_cat(i,j)=real(nfuel_cat0)
        enddo
    enddo
!$OMP CRITICAL(SFIRE_PHYS_CRIT)
    write(msg,'(a,i3)')'set_nfuel_cat: fuel initialized with category',nfuel_cat0
!$OMP END CRITICAL(SFIRE_PHYS_CRIT)
    call message(msg)
         
else if (ifuelread .eq. 1) then
!
!         make dependent on altitude (co mountains/forest vs. plains)
!          2000 m : 6562 ft   ;    1600 m: 5249 ft

!        ... user defines fuel category spatial variability ! param!
    do j=jfts,jfte
        do  i=ifts,ifte
            ! nfuel_cat(i,j)= 2     ! grass with understory ! jm does nothing
            !jm t1=zsf(i,j)*slngth/100.
            t1 = zsf(i,j)  ! this is in m
            if(t1.le.1524.)then   !  up to 5000 ft
                nfuel_cat(i,j)= 3  ! tall grass
            else if(t1.ge.1524. .and. t1.le.2073.)then  ! 5.0-6.8 kft.
                nfuel_cat(i,j)= 2  ! grass with understory
            else if(t1.ge.2073..and.t1.le.2438.)then  ! 6.8-8.0 kft.
                nfuel_cat(i,j)= 8  ! timber litter - 10 (ponderosa)
            else if(t1.gt.2438. .and. t1.le. 3354.) then ! 8.0-11.0 kft.
!                 ... could also be mixed conifer.
                nfuel_cat(i,j)= 10 ! timber litter - 8 (lodgepole)
            else if(t1.gt.3354. .and. t1.le. 3658.) then ! 11.0-12.0 kft
                nfuel_cat(i,j)= 1  ! alpine meadow - 1
            else if(t1.gt.3658. ) then  ! > 12.0 kft
                nfuel_cat(i,j)= 14 ! no fuel.
            endif
        enddo
    enddo

    call message('set_nfuel_cat: fuel initialized by altitude')
else

    call crash('set_nfuel_cat: bad ifuelread')
endif
!     .............end  load fuel categories (or constant) here.

end subroutine set_nfuel_cat            

!
!**********************
!            

subroutine fire_ros(ros_back,ros_wind,ros_slope, &
propx,propy,i,j,fp)

implicit none

! compute the wind speed and slope normal to the fireline and call fire_ros_cawfe

!*** arguments
real, intent(out)::ros_back,ros_wind,ros_slope ! rate of spread: backing, due to wind, due to slope
real, intent(in)::propx,propy
integer, intent(in)::i,j         ! node mesh coordinates
type(fire_params),intent(in)::fp

!*** local
real:: speed, tanphi ! windspeed and slope in the directino normal to the fireline
real::cor_wind,cor_slope,nvx,nvy,scale

!*** executable

! make sure normal direction is size 1
!scale=sqrt(propx*propx+propy*propy)+tiny(scale)
scale=1.
nvx=propx/scale
nvy=propy/scale
if (fire_advection.ne.0) then ! from flags in module_fr_sfire_util
    ! wind speed is total speed 
    speed =  sqrt(fp%vx(i,j)*fp%vx(i,j)+ fp%vy(i,j)*fp%vy(i,j))+tiny(speed)
    ! slope is total slope
    tanphi = sqrt(fp%dzdxf(i,j)*fp%dzdxf(i,j) + fp%dzdyf(i,j)*fp%dzdyf(i,j))+tiny(tanphi)
    ! cos of wind and spread, if >0
    cor_wind =  max(0.,(fp%vx(i,j)*nvx + fp%vy(i,j)*nvy)/speed)
    ! cos of slope and spread, if >0
    cor_slope = max(0., (fp%dzdxf(i,j)*nvx + fp%dzdyf(i,j)*nvy)/tanphi)
else
    ! wind speed in spread direction
    speed =  fp%vx(i,j)*nvx + fp%vy(i,j)*nvy
    ! slope in spread direction
    tanphi = fp%dzdxf(i,j)*nvx + fp%dzdyf(i,j)*nvy
    cor_wind=1.
    cor_slope=1.
endif

call fire_ros_cawfe(ros_back,ros_wind,ros_slope, &
speed,tanphi,cor_wind,cor_slope,i,j,fp)

end subroutine fire_ros

!
!***
!

subroutine fire_ros_cawfe(ros_back,ros_wind,ros_slope, &
speed,tanphi,cor_wind,cor_slope,i,j,fp)

implicit none

! find rate of spread from wind speed and slope
! copied from wf2_janice 
! with the following changes ONLY: 
!   0.5*(speed + abs(speed)) -> max(speed,0.)
!   index l -> j 
!   took out some prints
!   argument fuelloadm never used??
!   not using nfuel_cat here - cell info was put into arrays passed as arguments
!       in include file to avoid transcription errors when used elsewhere
!   betaop is absorbed in phiwc, see module_fr_sfire_model/fire_startup
!   return the backing, wind, and slope contributions to the rate of spread separately
!       because they may be needed to take advantage of known wind and slope vectors.
!       They should add up to get the total rate of spread.
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     ... calculates fire spread rate with mcarthur formula or Rothermel
!           using fuel type of fuel cell
!
!      
!         m/s =(ft/min) *.3048/60. =(ft/min) * .00508   ! conversion rate
!         ft/min = m/s * 2.2369 * 88. = m/s *  196.850 ! conversion rate
!      
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!*** arguments
real, intent(out)::ros_back,ros_wind,ros_slope ! rate of spread: backing, due to wind, due to slope
real, intent(in)::speed,tanphi,cor_wind,cor_slope
integer, intent(in)::i,j         ! node mesh coordinates
type(fire_params),intent(in)::fp

!*** local
real:: umid, phis, phiw, spdms, umidm, excess, tanphim
real, parameter::ros_max=6.
character(len=128)msg

!*** executable

if (.not. fp%ischap(i,j) > 0.) then            ! if not chaparral, do not test for .eq. 0 for speed
    if (ibeh .eq. 1) then                ! use Rothermel formula
!       ... if wind is 0 or into fireline, phiw = 0, &this reduces to backing ros.
        spdms = max(speed,0.)          ! 
        umidm = min(spdms,30.)       ! max input wind spd is 30 m/s   !param!
        umid = umidm * 196.850                    ! m/s to ft/min
        !  eqn.: phiw = c * umid**bbb(i,j) * (fp%betafl(i,j)/betaop)**(-e) ! wind coef
        ros_wind = fp%phiwc(i,j) * (umid**fp%bbb(i,j)) ! wind coef
        tanphim=max(tanphi,0.0)
        tanphim=min(tanphim,5.0)  ! jm
        ! phis = 5.275 *(fp%betafl(i,j))**(-0.3) *tanphim**2   ! slope factor
        ros_slope = fp%phisc(i,j) *tanphim**2   ! slope factor
        ! rosm = fp%r_0(i,j)*(1. + phiw + phis)  * .00508 ! spread rate, m/s
        ros_back = fp%r_0(i,j)
    elseif(ibeh.eq.2)then ! for testing only, spread rate = wind but not < 0
        ros_back = 0.
        ros_wind = max(speed,0.)
        ros_slope= 0.
    elseif(ibeh.eq.3)then ! for testing only, spread rate = wind but not < 0
        ros_back = 0.
        ros_wind = speed
        ros_slope= 0.
    elseif(ibeh.eq.0)then                                  ! MacArthur formula (Australian)
        ! rosm = 0.18*exp(0.8424*max(speed,0.))
        ros_back = 0.18
        ros_wind = 0.18*exp(0.8424*max(speed,0.)) - ros_back
        ros_slope =0.
        ! note: ros = ros_back + ros_wind + ros_slope
    else ! error, but prevent unintialized variables
        ! just so that there is something there
        ros_back=-999.
        ros_wind=-999.
        ros_slope=-999.
    endif
!
else   ! chaparral model from Clark et al 2004
!        .... spread rate has no dependency on fuel character, only windspeed.
    spdms = max(speed,0.)      
    ! rosm = 1.2974 * spdms**1.41       ! spread rate, m/s
    ! note: backing ros is 0 for chaparral without setting nozero value below
    !sp_n=.03333  
    ! chaparral backing fire spread rate 0.033 m/s   ! param!
    !rosm= max(rosm, sp_n)   ! no less than backing r.o.s.

    ros_back=.03333    ! chaparral backing fire spread rate 0.033 m/s   ! param!
    ros_wind = 1.2974 * spdms**1.41       ! spread rate, m/s
    ros_wind = max(ros_wind, ros_back)-ros_back
    ros_slope =0.

endif
!  multiply by the correction factors (from angle calculations)
ros_wind=ros_wind*cor_wind
ros_slope=ros_slope*cor_slope
!
!     ----------note!  put an 6 m/s cap on max spread rate -----------
! rosm= min(rosm, 6.)         ! no faster than this cap   ! param !

excess = ros_back + ros_wind + ros_slope - ros_max

if (excess > 0.)then
    ! take it out of wind and slope in proportion
    ros_wind = ros_wind - excess*ros_wind/(ros_wind+ros_slope)
    ros_slope = ros_slope - excess*ros_slope/(ros_wind+ros_slope)
endif


!     ... to rescale to veloc. carried by model, mult x (svel*snorm(1,3))= .1
!jm: huh ???
!     fire_ros = 0.1*rosm
!
!write(msg,*)i,j,' speed=',speed,' tanphi',tanphi,' ros=',ros_back,ros_wind,ros_slope
!call message(msg)
      return

contains
real function nrm2(u,v)
real, intent(in)::u,v
nrm2=sqrt(u*u+v*v)
end function nrm2

end subroutine fire_ros_cawfe

subroutine fire_risk(fp,               &
        ifms,ifme,jfms,jfme,                      &  ! memory dims
        ifts,ifte,jfts,jfte,                      &  ! tile dims
        nfuel_cat,                                &
        f_ros0,f_rosx,f_rosy,f_ros,               &  ! fire spread diagnostic variables
        f_int,f_lineint,f_lineint2)                  ! fireline intensities for danger rating

!*** arguments
type(fire_params), intent(in)::fp
integer, intent(in)::                             &
        ifms,ifme,jfms,jfme,                      &  ! memory dims
        ifts,ifte,jfts,jfte                          ! tile dims
real, intent(in), dimension(ifms:ifme,jfms:jfme) :: nfuel_cat 
real, intent(out), dimension(ifms:ifme,jfms:jfme) :: &
        f_ros0,f_rosx,f_rosy,f_ros,               &  ! fire spread diagnostic variables
        f_int,f_lineint,f_lineint2                   ! fire intensities for danger rating

!*** local
integer:: i,j,k
real:: cor_wind=1.,cor_slope=1.,dt_fake=1.
real:: ros_back,ros_wind,ros_slope,speed,tanphi,front_speed,ros_x,ros_y

!*** executable

do j=jfts,jfte
  do i=ifts,ifte
!   compute the fire spread rate and vector

    ! wind speed is total speed 
    speed =  sqrt(fp%vx(i,j)*fp%vx(i,j)+ fp%vy(i,j)*fp%vy(i,j))+tiny(speed)
    ! slope is total slope
    tanphi = sqrt(fp%dzdxf(i,j)*fp%dzdxf(i,j) + fp%dzdyf(i,j)*fp%dzdyf(i,j))+tiny(tanphi)

    call fire_ros_cawfe(ros_back,ros_wind,ros_slope, &
      speed,tanphi,cor_wind,cor_slope,i,j,fp)

    ros_x = ros_wind * fp%vx(i,j)/speed + ros_slope * fp%dzdxf(i,j)/tanphi ! x direction component
    ros_y = ros_wind * fp%vy(i,j)/speed + ros_slope * fp%dzdyf(i,j)/tanphi ! y direction component
   
!   store to out
    f_ros0(i,j) = ros_back      ! direction-less spread rate component
    f_rosx(i,j) = ros_x
    f_rosy(i,j) = ros_y

    ! max fire front speed in this location (m/s)
    f_ros(i,j) = ros_back + sqrt(ros_x*ros_x + ros_y*ros_y)

  enddo
enddo

call fire_intensity(fp,                       &  ! fuel properties
        ifms,ifme,jfms,jfme,                      &  ! memory dims
        ifts,ifte,jfts,jfte,                      &  ! tile dims
        ifms,ifme,jfms,jfme,                      &  ! f_ros dims
        f_ros,nfuel_cat,                          & !in
        f_lineint,f_lineint2,f_int)                 ! fireline intensities out

end subroutine fire_risk

!
!***
!

subroutine fire_intensity(fp,                     &  ! fuel params
        ifms,ifme,jfms,jfme,                      &  ! memory dims
        ifts,ifte,jfts,jfte,                      &  ! tile dims
        irms,irme,jrms,jrme,                      &  ! memory dims for ros
        ros,nfuel_cat,                            &  ! rate of spread in
        fibyram,filimit,f_int)                       ! intensities out

!*** arguments
type(fire_params), intent(in)::fp
integer, intent(in)::                             &
        ifms,ifme,jfms,jfme,                      &  ! memory dims
        ifts,ifte,jfts,jfte,                      &  ! tile dims
        irms,irme,jrms,jrme                         ! memory dims for ros
real, intent(in), dimension(irms:irme,jrms:jrme) :: ros ! in rate of spread
real, intent(in), dimension(ifms:ifme,jfms:jfme) :: nfuel_cat 
real, intent(out), dimension(ifms:ifme,jfms:jfme) :: &
        fibyram,filimit                             ! out fireline intensities
real, intent(out), dimension(ifms:ifme,jfms:jfme), optional :: f_int ! fire intensity (J/m^2/s)

!*** local
integer:: i,j,k
real, dimension(ifts:ifte,jfts:jfte):: rate_frac
real:: dt_fake=1.

!*** executable

call heat_fluxes(dt_fake,fp,                       &
        ifms,ifme,jfms,jfme,                      &  ! memory dims
        ifts,ifte,jfts,jfte,                      &  ! tile dims
        irms,irme,jrms,jrme,                      &  ! ros dims
        fp%fgip,ros,                              & !in
        fibyram)                                    !out

! multiply by fuel fraction consumed in flaming zone for the category
do j=jfts,jfte
    do i=ifts,ifte
       k=int( nfuel_cat(i,j) )
       fibyram(i,j)=fibyram(i,j)*ffw(k)
    enddo
enddo
    ! fuel fraction loss per fire front unit length traveled per unit time
    ! burn_rate(i,j) = 0.5 * front_speed / fp%fuel_time(i,j) 

    ! fireline element of length ds moves in time dt by front_speed * dt covering area ds * dt * front_speed (m^2)
    ! after time dt the fuel fraction decrease is 0 at the leading edge and dt/fuel_time at the trailing edge 
    ! so the average fuel consumption over this zone is 0.5 * dt/fuel_time  (1)
    ! fuel load is fgip (kg/m^2)
    ! and the amount of fuel burned fireline length ds travels over time dt is 0.5 fgip * ds * dt^2 * front_speed/fuel_time (kg)
    ! note: dt because 1. it travels more 2. it burns longer
    ! fgip*burn_rate_frac(i,j) = (kg/m^2) * (m/s^2) = kg/m/s^2

    ! http://www.forestencyclopedia.net/p/p487
    ! H = I*w*r  = (J/kg) * (kg/m^2) * (m/s) = J/m/s
    ! fuel fraction loss per fire front unit length traveled 

do j=jfts,jfte
    do i=ifts,ifte
       rate_frac(i,j)=0.5*ros(i,j)/fp%fuel_time(i,j)
    enddo
enddo

! multiply by heat contents * fuel load
call heat_fluxes(dt_fake,fp,                       &
        ifms,ifme,jfms,jfme,                      &  ! memory dims
        ifts,ifte,jfts,jfte,                      &  ! tile dims
        ifts,ifte,jfts,jfte,                      &  ! rate_frac dims
        fp%fgip,rate_frac,                        & !in
        filimit)                                    !out

if(present(f_int))then
  do j=jfts,jfte
    do i=ifts,ifte
       k=int( nfuel_cat(i,j) )
       ! in time tr the fraction of fuel consumed is ffw=1-exp(-tr/fuel_time)
       ! giving flame residence time tr = - log(1-ffw)*fuel_time
       ! fire intensity is heat contents * fuel load * fraction consumed / flame residence time
       ! J/kg * kg/m^2 * 1 / s = J/m^2/s
       ! this is averaged over the flame residence time, 
       rate_frac(i,j)=ffw(k)/(fp%fuel_time(i,j)*(-log(1.-ffw(k))))
    enddo
  enddo

  ! multiply by heat contents * fuel load J/m^2

  call heat_fluxes(dt_fake,fp,                      &
        ifms,ifme,jfms,jfme,                      &  ! memory dims
        ifts,ifte,jfts,jfte,                      &  ! tile dims
        ifts,ifte,jfts,jfte,                      &  ! rate_frac dims
        fp%fgip,rate_frac,                        & !in
        f_int)                                    !out

endif

end subroutine fire_intensity

!*** executable

end module module_fr_sfire_phys
