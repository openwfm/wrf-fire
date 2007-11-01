module module_fr_sfire_phys

use module_model_constants, only: cp,xlv

private fire_ros

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

! =============================================================================
! Standard 13 fire behavior fuel models (for surface fires), along with some
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
!  FUEL MODEL 14: no fuel??
!

   REAL, PARAMETER :: cmbcnst  = 17.433e+06             ! J/kg dry fuel
   REAL, PARAMETER :: hfgl     = 17.e4                  ! W/m^2
   REAL, PARAMETER :: fuelheat = cmbcnst * 4.30e-04     ! convert J/kg to BTU/lb
   REAL, PARAMETER :: fuelmc_g = 0.08                   ! set = 0 for dry ground fuel
   REAL, PARAMETER :: fuelmc_c = 1.00                   ! set = 0 for dry canopy
   REAL, PARAMETER :: bmst     = fuelmc_g/(1+fuelmc_g)
   !real, parameter :: xlv      = 2.5e6                  ! to make it selfcontained
   !real, parameter :: cp      =  7.*287./2              ! to make it selfcontained
   

   INTEGER, PARAMETER :: nfuelcats = 14
   INTEGER, DIMENSION( nfuelcats ), save :: ichap
   REAL   , DIMENSION( nfuelcats ), save :: weight,fgi,fci,fci_d,fct,fcbr, &
                                            fueldepthm,fueldens,fuelmce,   &
                                            savr,st,se

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

contains
   
subroutine init_fuel_cats
implicit none
!*** purpose: initialize fuel tables by constants
!*** arguments: none
!*** local
integer:: nf      
!*** executable

DO nf = 1,nfuelcats
    fci(nf) = (1.+fuelmc_c)*fci_d(nf)
    fcbr(nf) = fci_d(nf)/fct(nf)
END DO
      
end subroutine init_fuel_cats

!
!*******************
!

subroutine set_fire_params(ifts,ifte,jfts,jfte, &
                           ifms,ifme,jfms,jfme, &
                           fdx,fdy,             &
                           zsf,nfuel_cat,fuel_time  &
#                          include "fr_sfire_params_args.h" 
)

use module_fr_sfire_util
implicit none

!*** purpose: Set all fire model params arrays, constant values.

!*** arguments
integer, intent(in)::ifts,ifte,jfts,jfte                        ! fire mesh cell bounds
integer, intent(in)::ifms,ifme,jfms,jfme                        ! memory array bounds
real, intent(in):: fdx,fdy                                      ! fire mesh spacing
real, intent(in), dimension(ifms:ifme, jfms:jfme)::zsf          ! surface altitude
integer, intent(in),dimension(ifms:ifme, jfms:jfme)::nfuel_cat  ! fuel data
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time   ! fire params arrays
#define IN out  /* the fire param arrays are set here, elsewhere read only */
#include "fr_sfire_params_decl.h" 
#undef IN

!*** local

real::  fuelload, fueldepth, rtemp1, rtemp2, &
        qig, epsilon, rhob, wn, betaop, e, c, &
        xifr, etas, etam, a, gammax, gamma, ratio, ir, &
        fuelloadm
! real:: irm, phiw, tanphi, rosm, phis, slngth, umid, t1   ! jm: not used
integer:: i,j,k
logical:: err

!*** executable

! gradient of surface altitude, node based
call   meshdiffc_2d(ifts,ifte,jfts,jfte, &    ! mesh area used (in cells, end +1)
                   ifms,ifme,jfms,jfme, &       ! dimensions of input 
                   ifms,ifme,jfms,jfme, &       ! dimensions of the outputs
                   fdx,fdy,               &       ! mesh spacing
                   zsf,                 &       ! input
                   dzfsdx,dzfsdy) ! output

! fuel time constant, cell based
err=.false.
do j=jfts,jfte
    do i=ifts,ifte
        k=nfuel_cat(i,j)
        if(k.lt.1.or.k.gt.nfuelcats)then
            k=nfuelcats
            err=.true.
        endif
        fuel_time(i,j)=weight(k)/0.85 ! cell based
    enddo
enddo
if(err)call crash('set_fire_params: fuel category out of bounds')

do j=jfts,jfte+1
    do i=ifts,ifte+1
        
        ! do not understand calculations of stime in binit.m4
        ! set fuel time constant: weight=1000=>40% decrease over 10 min
        ! fuel decreases as exp(-t/fuel_time) 
        ! exp(-600*0.85/1000) = approx 0.6 

        ischap(i,j)=ichap(nfuel_cat(i,j))
        fgip(i,j)=fgi(nfuel_cat(i,j))

        ! end jm addition

        !
        !*** rest copied from wf2_janice/fire_startup.m4 with minimal changes
        !

        !     ...Settings of fire spread parameters from BEHAVE follows. These
        !        don't need to be recalculated later.
        
        fuelloadm= (1.-bmst) * fgi(nfuel_cat(i,j))  !  fuelload without moisture
        fuelload = fuelloadm * (.3048)**2 * 2.205    ! to lb/ft^2
        fueldepth = fueldepthm(nfuel_cat(i,j))/0.3048               ! to ft
        betafl(i,j) = fuelload/(fueldepth * fueldens(nfuel_cat(i,j)))! packing ratio
        betaop = 3.348 * savr(nfuel_cat(i,j))**(-0.8189)     ! optimum packing ratio
        qig = 250. + 1116.*fuelmc_g            ! heat of preignition, btu/lb
        epsilon = exp(-138./savr(nfuel_cat(i,j)) )    ! effective heating number
        rhob = fuelload/fueldepth    ! ovendry bulk density, lb/ft^3

        c = 7.47 * exp( -0.133 * savr(nfuel_cat(i,j))**0.55)    ! const in wind coef
        bbb(i,j) = 0.02526 * savr(nfuel_cat(i,j))**0.54                ! const in wind coef
        e = 0.715 * exp( -3.59e-4 * savr(nfuel_cat(i,j)))       ! const in wind coef
        phiwc(i,j) = c * (betafl(i,j)/betaop)**(-e)

        rtemp2 = savr(nfuel_cat(i,j))**1.5
        gammax = rtemp2/(495. + 0.0594*rtemp2)              ! maximum rxn vel, 1/min
        a = 1./(4.774 * savr(nfuel_cat(i,j))**0.1 - 7.27)   ! coef for optimum rxn vel
        ratio = betafl(i,j)/betaop
        gamma = gammax *(ratio**a) *exp(a*(1.-ratio)) !optimum rxn vel, 1/min

        wn = fuelload/(1 + st(nfuel_cat(i,j)))       ! net fuel loading, lb/ft^2
        rtemp1 = fuelmc_g/fuelmce(nfuel_cat(i,j))
        etam = 1.-2.59*rtemp1 +5.11*rtemp1**2 -3.52*rtemp1**3  !moist damp coef
        etas = 0.174* se(nfuel_cat(i,j))**(-0.19)                ! mineral damping coef
        ir = gamma * wn * fuelheat * etam * etas  !rxn intensity,btu/ft^2 min
        ! jm irm = ir * 1055./( 0.3048**2 * 60.) * 1.e-6     !for mw/m^2
        ! jm: irm set but never used??

        xifr = exp( (0.792 + 0.681*savr(nfuel_cat(i,j))**0.5) &
            * (betafl(i,j)+0.1)) /(192. + 0.2595*savr(nfuel_cat(i,j))) ! propagating flux ratio

!        ... r_0 is the spread rate for a fire on flat ground with no wind.

        r_0(i,j) = ir*xifr/(rhob * epsilon *qig)    ! default spread rate in ft/min
        if (nfuel_cat(i,j) .eq. 14) r_0(i,j)=0.     ! no fuel, no spread.
    enddo
enddo

end subroutine set_fire_params

!
!*******************
!

subroutine init_model_no_fire(ifts,ifte,jfts,jfte, &
                             ifms,ifme,jfms,jfme, &
                             fuel_frac,lfn,tign)               
implicit none
             
!*** purpose: initialize model to no fire

!*** arguments
integer, intent(in):: ifts,ifte,jfts,jfte   ! fire domain bounds
integer, intent(in):: ifms,ifme,jfms,jfme   ! array bounds
real, intent(out), dimension (ifms:ifme,jfms:jfme) :: & 
                   fuel_frac,lfn,tign       ! model state

!*** calls
intrinsic huge
                        
!*** local
integer:: i,j
real, parameter:: faraway = huge(tign)

do j=jfts,jfte
    do i=ifts,ifte
        fuel_frac(i,j)=1.  ! fuel at start is 1 by definition
    enddo
enddo

do j=jfts,jfte+1
    do i=ifts,ifte+1
        tign(i,j) = faraway ! ignition way in future
        lfn(i,j) = 1        ! no fire 
    enddo
enddo

end subroutine init_model_no_fire

!
!******************
!

subroutine ignite_fire( ifts,ifte,jfts,jfte,                      &
                        ifms,ifme,jfms,jfme,                      &
                        time_ign,ctri,ctrj,diam,fdx,fdy,lfn,tign)
implicit none

!*** purpose: ignite a circular fire 

!*** arguments
integer, intent(in):: ifts,ifte,jfts,jfte   ! fire domain bounds
integer, intent(in):: ifms,ifme,jfms,jfme   ! array bounds
real, intent(in):: time_ign                 ! the ignition time of the fire
real, intent(in):: ctri,ctrj                ! mesh coordinates of the center of the fire
!                         dimensionless, if integers, the center is is a mesh node
real, intent(in):: diam                     ! diam of the ignited area (m)
real, intent(in):: fdx,fdy                  ! mesh spacing (m)
real, intent(out), dimension (ifms:ifme,jfms:jfme) :: & 
                   lfn, tign                ! level function, ignition time (state)
!*** calls
intrinsic huge
                        
!*** local
integer:: i,j
real, parameter:: faraway = huge(diam)

!jmtodo this will not work on tiles

do j=jfts,jfte+1      ! node based loops, hence the +1
    do i=ifts,ifte+1                   
        lfn(i,j)=sqrt(((i-ctri)*fdx)**2+((j-ctrj)*fdy)**2)-diam/2
        if(lfn(i,j)<=0) then
            tign(i,j)=time_ign  ! ignited now
        endif
    enddo
enddo
end subroutine ignite_fire

!
!**********************
!            

subroutine heat_fluxes(dt,                        &
        ifts,ifte,jfts,jfte,                      &
        ifms,ifme,jfms,jfme,                      &
        fgip,fuel_frac_burnt,                     & !in
        grnhft,grnqft)                              !out
implicit none

!*** purpose        
! compute the heat fluxes on the fire grid cells

!*** arguments
real, intent(in)::dt          ! dt  the fire time step (the fire model advances time by this)
integer, intent(in)::ifts,ifte,jfts,jfte,& 
        ifms,ifme,jfms,jfme   ! dimensions                   
real, intent(in),dimension(ifms:ifme,jfms:jfme):: fgip,fuel_frac_burnt
real, intent(out),dimension(ifms:ifme,jfms:jfme):: grnhft,grnqft

!*** local
integer::i,j
real:: dmass

!*** executable        
do j=jfts,jfte
    do i=ifts,ifte
         dmass =                     &     ! ground fuel dry mass burnt this call (kg/m^2)
             fgip(i,j)               &     ! init mass from fuel model no (kg/m^2)
             * fuel_frac_burnt(i,j)        ! fraction burned this call    (1)
         grnhft(i,j) = (dmass/dt)*(1.-bmst)*cmbcnst         ! J/m^2/sec
         grnqft(i,j) = (bmst+(1.-bmst)*.56)*(dmass/dt)*xlv  ! what the #!@* is that??
         ! xlv is defined in module_model_constants.. who knows that it is.. why .56 ??
    enddo
enddo

end subroutine heat_fluxes

!
!**********************
!            


subroutine set_nfuel_cat(   ifts,ifte,jfts,jfte,               &
                            ifms,ifme,jfms,jfme,               &
                            ifuelread,zsf,nfuel_cat)

use module_fr_sfire_util
implicit none

! set fuel distributions for testing
integer, intent(in)::   ifts,ifte,jfts,jfte,               &
                        ifms,ifme,jfms,jfme               

integer, intent(in)::ifuelread
real, intent(in), dimension(ifms:ifme, jfms:jfme)::zsf
integer, intent(out), dimension(ifms:ifme, jfms:jfme)::nfuel_cat

!*** local

! parameters to control execution
integer:: nfuel_cat0,i,j,iu1
real:: t1


if (ifuelread .eq. 0) then
!
    nfuel_cat0= 8             ! param! set category
    do j=jfts,jfte+1
        do  i=ifts,ifte+1
            nfuel_cat(i,j)=nfuel_cat0
        enddo
    enddo
         
else if (ifuelread .eq. 1) then
!
!         make dependent on altitude (co mountains/forest vs. plains)
!          2000 m : 6562 ft   ;    1600 m: 5249 ft

!        ... user defines fuel category spatial variability ! param!
    do j=jfts,jfte+1
        do  i=ifts,ifte+1
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

else if (ifuelread .eq. 2) then

! read it somewhere else?? how to read data in wrf and preserve from one call 
! to the next? jm

!        ...  read fuel files

    iu1=10
         open(iu1,file='fuel_layer',status='unknown',form='formatted')
    do j=jfts,jfte+1
        do  i=ifts,ifte+1
            read(iu1,'(i2)') nfuel_cat(i,j)
!           ... if no fuel category specified (i.e. '99'), set to '14',
!               which (in the current 13 category nffl category system)
!               is 'no fuel'
            if (nfuel_cat(i,j).lt.1) nfuel_cat(i,j)=14     ! not generalized
            if (nfuel_cat(i,j).gt.nfuelcats) nfuel_cat(i,j)=14     ! not generalized
        enddo
    enddo

    close (iu1)

else
    call crash('set_nfuel_cat: bad ifuelread')
endif
!     .............end  load fuel categories (or constant) here.

end subroutine set_nfuel_cat            

!
!**********************
!            

subroutine normal_spread(t,dx,dy,n,rii,rjj,&
ids,ide,jds,jde,ims,ime,jms,jme,&
nvx,nvy,scale,ros &
#include "fr_sfire_params_args.h"
)

use module_fr_sfire_util
implicit none
!***  purpose: the speed function

!*** arguments
!  see module_fr_sfire_prop for argument description
real, intent(in)::t,dx,dy  ! currently not used
integer, intent(in)::n,ids,ide,jds,jde,ims,ime,jms,jme
real,intent(in),dimension(ims:ime,jms:jme)::nvx,nvy,scale
real, intent(in),dimension(n)::rii,rjj
real,intent(out),dimension(n)::ros
#include "fr_sfire_params_decl.h"

!*** calls
intrinsic nint,max,min

!***  internal
integer i,j,k
real tanphi
real speed
real,dimension(ids:ide+1,jds:jde+1)::ros_all

!*** executable

!  compute speed at all nodes
do j=jds,jde+1
    do i=ids,ide+1

        ! wind speed in direction of spread
        speed =  vx(i,j)*nvx(i,j) + vy(i,j)*nvy(i,j)
        
        ! slope in direction of spread
        tanphi =  dzfsdx(i,j)*nvx(i,j) + dzfsdy(i,j)*nvy(i,j)
  
        ! get rate of spread from wind speed and slope
         
        !ros_all(i,j) = max(speed,0.0)  ! debug only 
        ros_all(i,j) = fire_ros(speed,tanphi,i,j &
#       include "fr_sfire_params_args.h"
        )

    enddo
enddo

! interpolate to the requested points
do k=1,n
    ros(k)=interp(ids,ide,jds,jde,ids,ide+1,jds,jde+1,rii(k),rjj(k),ros_all)
enddo
    
end subroutine normal_spread

!
!**********************
!            

real function fire_ros(speed,tanphi,i,j &
#include "fr_sfire_params_args.h"
)
implicit none
#include "fr_sfire_params_decl.h"

! copied from wf2_janice 
! changes only:
!   required by f90
!   0.5*(speed + abs(speed)) -> max(speed,0.)
!   index l -> j 
!   the indices are node not cell but that changes nothing
!   took out some prints
!   made into a function
!   argument fuelloadm never used??
!   not using nfuel_cat here - cell info was put into module arrays
!   betaop is module variable instead of local, in  case when needed 
!       (set in module_fr_sfire_model/fire_startup)
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!     ... calculates fire spread rate with mcarthur formula or behave
!           using fuel type of fuel cell
!
!      
!         m/s =(ft/min) *.3048/60. =(ft/min) * .00508   ! conversion rate
!         ft/min = m/s * 2.2369 * 88. = m/s *  196.850 ! conversion rate
!      
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

!*** arguments
real, intent(in):: speed, tanphi ! windspeed and slope in the directino normal to the fireline
integer, intent(in)::i,j         ! node mesh coordinates

!*** local
real:: umid, phis, phiw, rosm, spdms, umidm, sp_n
!real:: e,  c
integer:: ibeh
parameter(ibeh=1)

!*** executable

if (ischap(i,j) .eq. 0) then            ! not chaparral
    if (ibeh .eq. 1) then                ! behave
!       ... if wind is 0 or into fireline, phiw = 0, &this reduces to backing ros.
        spdms = max(speed,0.)          ! unscale model to m/s 10*.5
        umidm = min(spdms,10.)       ! max input wind spd is 10 m/s   !param!
        umid = umidm * 196.850                    ! m/s to ft/min
        !  eqn.: phiw = c * umid**bbb(i,j) * (betafl(i,j)/betaop)**(-e) ! wind coef
        phiw = umid**bbb(i,j) * phiwc(i,j) ! wind coef
        phis=0  !*** jm added ??
        if (tanphi .gt. 0.) then
            phis = 5.275 *(betafl(i,j))**(-0.3) *tanphi**2   ! slope factor
        endif
        rosm = r_0(i,j)*(1. + phiw + phis)  * .00508 ! spread rate, m/s
            
        !write (6,'(x,a,2i4,4(x,e12.3) )') 'i,l,rosm,r0,phiw,phis=', &
        !   i,l,rosm,r_0(i,j),phiw,phis
        !if (rosm .gt. 1) write (6,*) 'speed=',speed,' tanphi=',tanphi
!
    else                                   !macarthur formula
        rosm = 0.18*exp(0.8424*max(speed,0.))
    endif
!
else   ! chaparral
!        .... spread rate has no dependency on fuel character, only windspeed.
    spdms = max(speed,0.)       ! unscale model to m/s 10*.5
    rosm = 1.2974 * spdms**1.41       ! spread rate, m/s
    ! note: backing ros is 0 for chaparral without setting nozero value below
    sp_n=.03333    ! chaparral backing fire spread rate 0.033 m/s   ! param!
    rosm= max(rosm, sp_n)   ! no less than backing r.o.s.
endif
!
!     ----------note!  put an 6 m/s cap on max spread rate -----------
rosm= min(rosm, 6.)         ! no faster than this cap   ! param !

!     ... to rescale to veloc. carried by model, mult x (svel*snorm(1,3))= .1
!jm: huh ???
      fire_ros = 0.1*rosm
!
      return
end function fire_ros 

end module module_fr_sfire_phys
