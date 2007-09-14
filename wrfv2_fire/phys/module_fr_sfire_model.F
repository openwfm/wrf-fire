module module_fr_sfire_model
!
!*** Jan Mandel September 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
contains

subroutine sfire_model ( &
                 ifts,ifte,jfts,jfte,               &
                 ifms,ifme,jfms,jfme,               &
                 kw,                                &
                 time_start,dt,                     &
                 fdx,fdy,                           &
                 nfuel_cat,zsf,vfx,vfy,             &
                 phi,tign,fuel_frac,                &
                 grnhft,grnqft) 

! This subroutine implements the fire spread model.
! All quantities are on the fire grid. It inputs
! winds on given on the nodes of the fire grid
! and outputs the heat fluxes on the cells of the fire grid.
! This subroutine has no knowledge of the atmospheric model.

use module_fr_sfire_core
use module_fr_sfire_util
use module_fr_sfire_speed
use module_fr_cawfe_fuel
use module_model_constants
implicit none

!*** arguments

! input

INTEGER, INTENT(in) :: ifts,ifte,jfts,jfte  ! fire domain bounds
INTEGER, INTENT(in) :: ifms,ifme,jfms,jfme  ! fire memory array bounds
INTEGER, INTENT(in) :: kw                   ! number of vertical layers of wind (1=surface)
REAL,INTENT(in) :: time_start,dt            ! starting time, time step
REAL,INTENT(in) :: fdx,fdy                  ! dx,dy on innermost atm mesh
INTEGER, INTENT(in), dimension(ifms:ifme,jfms:jfme):: &
    nfuel_cat                               ! fuel category (cell)
REAL, INTENT(in), dimension(ifms:ifme,jfms:jfme,kw):: &
    vfx,vfy                                 ! wind m/s (node)
    
! state

REAL, INTENT(inout), dimension(ifms:ifme,jfms:jfme,kw):: &
    phi   , &                               ! level function: fire is where phi<0 (node)
    tign  , &                               ! absolute time of ignition (node)
    fuel_frac                               ! fuel fraction (node), currently redundant
    
! output
REAL, INTENT(out), dimension(ifms:ifme,jfms:jfme):: &
    grnhft,grnqft                           ! heat fluxes J/m^2/s  (cell)             
            
!*** local

integer::i,j        
! fire grid size declared in module_fr_sfire_speed
real, dimension(ifms:ifme, jfms:jfme):: & 
    fuel_frac_start,& ! the fuel fraction at start
    fuel_time, &      ! derived data
    fuel_frac_burnt,& ! 
    zf,zsf            ! terrain height
    
real, target, dimension(ifms:ifme, jfms:jfme):: &  ! arrays for the speed function
    t_vx,t_vy,     &             ! wind 
    t_dzfsdx,t_dzfsdy,&          ! terrain gradient
    t_bbb,t_betafl,t_phiwc,t_r_0 ! fuel model params
integer, target, dimension(ifms:ifme, jfms:jfme)::t_ischap ! chapparall switch
real,  dimension(ifms:ifme, jfms:jfme)::time_fuel,fgip  ! fuel burns off as exp(t/time_fuel)
real:: dmass

!*** executable

! pass local automatic arrays to module_fr_sfire_speed by associating pointers
! cannot use the allocate statement because of wrf restrictions
vx      => t_vx
vy      => t_vy
dzfsdx  => t_dzfsdx
dzfsdy  => t_dzfsdy
bbb     => t_bbb
betafl  => t_betafl
phiwc   => t_phiwc
r_0     => t_r_0
ischap  => t_ischap


!***** set up the model data arrays
! copy the ground winds - different every call
! We cannot simply compute the spread rate here because that will change with the
! angle of the wind and the direction of propagation, thus it is done in subroutine
! normal_spread at each fire time step. Instead, we set some quantities that 
! subroutine normal_spread may use.
! used in the speed function and possibly as background advection field in propagation
do j=jfts,jfte+1
    do i=ifts,ifte+1
        vx(i,j)=vfx(i,j,1)
        vy(i,j)=vfy(i,j,1)
    enddo
enddo

! compute only once fuel parameter arrays, these do not change
call fire_startup(ifts,ifte,jfts,ifte, &
                  ifms,ifme,jfms,jfme, &
                  fdx,fdy,zsf,fuel_time,fgip)


! advance the model from time_start to time_start+dt
! return the fuel fraction burnt this call in each fire cell
! will call module_fr_sfire_speed%%normal_speed for propagation speed
! uses vx,vy,r as a background propagation field 
call sfire_core(     &
    ifts,ifte,jfts,jfte,                      &
    ifms,ifme,jfms,jfme,                      &
    time_start,dt,fdx,fdy,fuel_time,          &
    phi,tign,fuel_frac,fuel_frac_burnt ) 

! compute the heat fluxes on the fire grid cells
do j=jfts,jfte
    do i=ifts,ifte
         dmass =                     &     ! ground fuel mass burnt this call (kg/m^2)
             fgip(i,j)               &     ! init mass from fuel model no (kg/m^2)
             * fuel_frac_burnt(i,j)        ! fraction burned this call    (1)
         grnhft(i,j) = (dmass/dt)*(1.-bmst)*cmbcnst         ! J/m^2/sec
         grnqft(i,j) = (bmst+(1.-bmst)*.56)*(dmass/dt)*xlv  ! what the #!@* is that??
         ! xlv is defined in module_model_constants.. who knows that it is.. why .56 ??
    enddo
enddo

end subroutine sfire_model

!
!*****************
!

subroutine fire_startup(ifts,ifte,jfts,jfte, &
                        ifms,ifme,jfms,jfme, &
                        fdx,fdy,zsf,fuel_time,fgip)
use module_fr_cawfe_fuel
use module_fr_sfire_speed
implicit none

!*** purpose: set arrays that do not change over time
! copied from wf2_janice/fire_startup.m4 with minimal changes

!*** arguments
integer, intent(in)::ifts,ifte,jfts,jfte     ! fire mesh cell bounds
integer, intent(in)::ifms,ifme,jfms,jfme     ! memory array bounds
real, intent(in):: fdx,fdy                   ! fire mesh spacing
real, intent(in), dimension(ifms:ifme, jfms:jfme)::zsf ! surface altitude
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time, & ! time const of burn
                   fgip                      ! FGI
!*** local

! temporary
real:: umid, fuelload, fueldepth, rtemp1, rtemp2, tanphi, &
         qig, epsilon, rhob, phis, wn, betaop, e, c, phiw, &
         xifr, etas, etam, a, gammax, gamma, ratio, ir, irm, rosm, t1, &
         slngth, fuelloadm
integer:: i,j,iu1
! what the $#% is slngth??
      
integer, dimension(ifts:ifte,jfts:jfte)::nfuel_cat ! fuel distribution
! Hey we need to make up our mind - do we want to carry around nfuel_cat or
! the spatial arrays of its values?? The original code is mostly carrying the
! spatial arrays but slips in few places and uses nfuel_cat anyway. Here 
! nfuel_cat(i,j) is *local* to this subroutine. Only the values of the fuel
! parameters at (i,j) are known outside. This will allow more generality in future,
! if needed, and it is consistent.

! parameters to control execution
integer:: ifuelread,nfuel_cat0

!*** executable

! gradient of surface altitude, node based
do j=jfts,jfte+1
    do i=ifts,ifte+1
        dzfsdx(i,j)=0.5*(zsf(i+1,j)-zsf(i-1,j))/fdx ! terrain slope in x direction
        dzfsdy(i,j)=0.5*(zsf(i,j+1)-zsf(i,j-1))/fdy ! terrain slope in y direction
    enddo
enddo

! set fuel distributions for testing

ifuelread = 0
if (ifuelread .eq. 0) then
!
    nfuel_cat0= 8             ! param! set category
    do j=jfte,jfte
        do  i=ifts,ifte
            nfuel_cat(i,j)=nfuel_cat0
        enddo
    enddo
         
else if (ifuelread .eq. 1) then
!
!         make dependent on altitude (co mountains/forest vs. plains)
!          2000 m : 6562 ft   ;    1600 m: 5249 ft

!        ... user defines fuel category spatial variability ! param!
    do j=jfte,jfte
        do  i=ifts,ifte
            nfuel_cat(i,j)= 2     ! grass with understory
            t1=zsf(i,j)*slngth/100.
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
    do j=jfte,jfte
        do  i=ifts,ifte
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
    call crash('fire_startup: bad ifuelread')
endif
!     .............end  load fuel categories (or constant) here.



!     ...Settings of fire spread parameters from BEHAVE follows. These
!        don't need to be recalculated later.
!
do j=jfts,jfte+1
    do i=ifts,ifte+1
        
        ! jm: do not understand calculations of stime in binit.m4
        ! set fuel time constant: weight=1000=>40% decrease over 10 min
        ! fuel decreases as exp(-t/fuel_time) 
        ! exp(-600*0.85/1000) = approx 0.6 
        fuel_time(i,j)=weight(nfuel_cat(i,j))/0.85
        ischap(i,j)=ichap(nfuel_cat(i,j))  ! nfuel_cat is UNKNOWN outside of here
        fgip(i,j)=fgi(nfuel_cat(i,j))
        ! end jm addition
        
        ! orig code from wf2_janice/fire_startup.m4 follows   
        fuelloadm= (1.-bmst) * fgi(nfuel_cat(i,j))  !  fuelload without moisture
        fuelload = fuelloadm * (.3048)**2 * 2.205    ! to lb/ft^2
        fueldepth = fueldepthm(nfuel_cat(i,j))/0.3048               ! to ft
        betafl(i,j) = fuelload/(fueldepth * fueldens(nfuel_cat(i,j)))    ! packing ratio
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
        irm = ir * 1055./( 0.3048**2 * 60.) * 1.e-6     !for mw/m^2

        xifr = exp( (0.792 + 0.681*savr(nfuel_cat(i,j))**0.5) &
            * (betafl(i,j)+0.1)) /(192. + 0.2595*savr(nfuel_cat(i,j))) ! propagating flux ratio

!        ... r_0 is the spread rate for a fire on flat ground with no wind.

        r_0(i,j) = ir*xifr/(rhob * epsilon *qig)    ! default spread rate in ft/min
        if (nfuel_cat(i,j) .eq. 14) r_0(i,j)=0.     ! no fuel, no spread.
    enddo
enddo

end subroutine fire_startup
end module module_fr_sfire_model
