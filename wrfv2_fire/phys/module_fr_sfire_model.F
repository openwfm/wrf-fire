module module_fr_sfire_model
!
!*** Jan Mandel September 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
contains

subroutine sfire_model (initialize, ignition,       &
                 ifts,ifte,jfts,jfte,               &
                 ifms,ifme,jfms,jfme,               &
                 kw,                                &
                 time_start,dt,                     &
                 fdx,fdy,                           &
                 nfuel_cat,zsf,vfx,vfy,             &
                 lfn,tign,fuel_frac,                &
                 grnhft,grnqft) 

! This subroutine implements the fire spread model.
! All quantities are on the fire grid. It inputs
! winds given on the nodes of the fire grid
! and outputs the heat fluxes on the cells of the fire grid.
! This subroutine has no knowledge of any atmospheric model.

use module_fr_sfire_params
use module_fr_sfire_core
use module_fr_sfire_util
use module_fr_sfire_fuel
implicit none

!*** arguments

! input

integer, intent(in) :: initialize, ignition ! if not 0, do this and proceed with timestep
INTEGER, INTENT(in) :: ifts,ifte,jfts,jfte  ! fire domain bounds
INTEGER, INTENT(in) :: ifms,ifme,jfms,jfme  ! fire memory array bounds
INTEGER, INTENT(in) :: kw                   ! number of vertical layers of wind (1=surface)
REAL,INTENT(in) :: time_start,dt            ! starting time, time step
REAL,INTENT(in) :: fdx,fdy                  ! spacing of the fire mesh
integer, intent(in), dimension(ifms:ifme, jfms:jfme)::nfuel_cat
REAL, INTENT(in), dimension(ifms:ifme,jfms:jfme,kw):: & 
    zsf,   &                                ! terrain         
    vfx,vfy                                 ! wind m/s (node)
    
! state

REAL, INTENT(inout), dimension(ifms:ifme,jfms:jfme,kw):: &
    lfn   , &                               ! level function: fire is where lfn<0 (node)
    tign  , &                               ! absolute time of ignition (node)
    fuel_frac                               ! fuel fraction (node), currently redundant
    
! output
REAL, INTENT(out), dimension(ifms:ifme,jfms:jfme):: &
    grnhft,grnqft                           ! heat fluxes J/m^2/s  (cell)             
            
!*** local

integer::i,j        
! fire grid size declared in module_fr_sfire_speed
real, dimension(ifms:ifme, jfms:jfme)::    fuel_frac_burnt
real:: dmass,ctri,ctrj,diam
integer, parameter:: printl=1
character(len=128)msg

!*** executable

if (initialize.ne.0) then

    ! we do this in violation of wrf coding conventions. will fix later.
    call allocate_fire_params(ifms,ifme,jfms,jfme)

    ! initialize all arrays that the model will not change later
    call set_fire_params(  ifts,ifte,jfts,ifte, &
                            ifms,ifme,jfms,jfme, &
                            fdx,fdy,             &
                            zsf,nfuel_cat)
                        
    ! initialize variable model arrays to no fire
    call init_model_no_fire(ifts,ifte,jfts,ifte, &
                            ifms,ifme,jfms,jfme, &
                            fuel_frac,lfn,tign)                            
                            
endif

if (ignition.ne.0) then
    ! fire in the middle
    ctri=(ifts+ifte+1)*0.5
    ctrj=(jfts+jfte+1)*0.5
    ! at least 6 by 6 cells but no less than 5 m
    diam = max(5.0,6*max(fdx,fdy))    
    call ignite_fire(   ifts,ifte,jfts,jfte,                      &
                        ifms,ifme,jfms,jfme,                      &
                        time_start,ctri,ctrj,diam,fdx,fdy,lfn,tign)
endif

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
! advance the model from time_start to time_start+dt
! return the fuel fraction burnt this call in each fire cell
! will call module_fr_sfire_speed%%normal_speed for propagation speed
! uses vx,vy,r as a background propagation field 
call sfire_core(     &
    ifts,ifte,jfts,jfte,                      &
    ifms,ifme,jfms,jfme,                      &
    time_start,dt,fdx,fdy,fuel_time,vx,vy,r_0,  &
    lfn,tign,fuel_frac,fuel_frac_burnt ) 

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

subroutine set_fire_params(ifts,ifte,jfts,jfte, &
                           ifms,ifme,jfms,jfme, &
                           fdx,fdy,             &
                           zsf,nfuel_cat)
use module_fr_sfire_fuel
use module_fr_sfire_params
use module_fr_sfire_util
implicit none

!*** purpose: set all fire model arrays that do not change over time

!*** arguments
integer, intent(in)::ifts,ifte,jfts,jfte     ! fire mesh cell bounds
integer, intent(in)::ifms,ifme,jfms,jfme     ! memory array bounds
real, intent(in):: fdx,fdy                   ! fire mesh spacing
real, intent(in), dimension(ifms:ifme, jfms:jfme)::zsf ! surface altitude
integer, intent(in), dimension(ifms:ifme, jfms:jfme)::nfuel_cat
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

subroutine set_nfuel_cat(   ifts,ifte,jfts,jfte,               &
                            ifms,ifme,jfms,jfme,               &
                            ifuelread,zsf,nfuel_cat)
use module_fr_sfire_fuel
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
            
end module module_fr_sfire_model
