! standalone moisture model test
program moisture_main
use module_fr_sfire_phys, only: advance_moisture, init_fuel_cats, moisture_classes
use module_fr_sfire_util, only: fire_print_msg 
implicit none

integer, parameter:: ims=1,ime=10,jms=1,jme=15,nfmc=5
integer::its=ims,ite=ime,jts=jms,jte=jme
real, dimension(ims:ime,1:nfmc,jms:jme)::fmc_gc,fmc_equi,fmc_tend
real, dimension(ims:ime,jms:jme)::t2,q2,psfc,t2_old,q2_old,psfc_old, &
   rainc,rainnc,rain_old
integer, parameter::nsteps=50
logical::initialize
integer::fmoist_init=2
real::moisture_dt,dt
integer::istep,i
real, dimension(nsteps)::t,q,p,rain

! test data
integer, parameter::zs=nsteps-10;
data dt/600./
data t/300.,300.,300.,305.,310., 305.,303.,300.,295.,290.,zs*285/
!data t/300.,300.,300.,305.,310., 290.,290.,290.,290.,280.,zs*270/
data q/nsteps*0.002/
data p/nsteps*1e5/
data rain/0.0,0.01,0.01,0.1,1.0, 2.0,3.0,5.0,5.0,6.0,zs*6.0/

!*** executable
fire_print_msg = 2

call init_fuel_cats(.true.)
print *,moisture_classes,' moisture classes'

do istep=1,nsteps

    if(istep.eq.1)then
        moisture_dt=0.  ! required on initialization
        initialize=.true.
    else
        moisture_dt=dt  !
        initialize=.false.
    endif

    write(*,*)'step',istep,' dt=',moisture_dt,' t=',t(istep),' q2=',q(istep),' p=',p(istep), &
        ' rain=',rain(istep)
 
    t2=t(istep)
    q2=q(istep)
    psfc=p(istep)
    rainc=0
    rainnc=rain(istep)
       
    call advance_moisture(    &
        initialize,                 & ! initialize timestepping. true on the first call at time 0, then false
        fmoist_init,                & ! initial moisture: 1=fuelmc_g, 2=equilibrium, else = none
        ims,ime,  jms,jme,          & ! memory dimensions
        its,ite,  jts,jte,          & ! tile dimensions
        nfmc,                       & ! dimension of moisture fields
        moisture_dt,                & ! timestep = time step time elapsed from the last call
        rainc, rainnc,              & ! accumulated rain
        t2, q2, psfc,               & ! temperature (K), vapor contents (kg/kg), pressure (Pa) at the surface
        rain_old,                   & ! previous value of accumulated rain
        t2_old, q2_old, psfc_old,   & ! previous values of the atmospheric state at surface
        fmc_gc,                     & ! fuel moisture by class, updated
        fmc_equi,                   & ! fuel moisture equilibrium by class, for diagnostics only
        fmc_tend                    & ! fuel moisture tendency by classe, for diagnostics only
        )

enddo

end program moisture_main
