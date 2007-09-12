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
   REAL, PARAMETER :: cmbcnst  = 17.433e+06             ! J/kg dry fuel
   REAL, PARAMETER :: hfgl     = 17.e4                  ! W/m^2
   REAL, PARAMETER :: fuelheat = cmbcnst * 4.30e-04     ! convert J/kg to BTU/lb
   REAL, PARAMETER :: fuelmc_g = 0.08                   ! set = 0 for dry ground fuel
   REAL, PARAMETER :: fuelmc_c = 1.00                   ! set = 0 for dry canopy
   REAL, PARAMETER :: bmst     = fuelmc_g/(1+fuelmc_g)

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
   
   ! ----- fuel moisture parameters
      subroutine init_fuel_cats
      integer nf      

      DO nf = 1,nfuelcats
         fci(nf) = (1.+fuelmc_c)*fci_d(nf)
         fcbr(nf) = fci_d(nf)/fct(nf)
      END DO
      
      end subroutine init_fuel_cats

END MODULE module_fr_cawfe_fuel
