! standalone moisture model test
program moisture_main
use module_fr_sfire_phys, only: advance_moisture, init_fuel_cats 

integer, parameter:: ims=1,ime=10,jms=1,jme=15,nfms=5
integer::its=ims,ite=ime,jts=jms,jte=jme
real, dimension(ims:ime,1:nfms,jms:jme)::fmc_gc,fmc_equi,fmc_tend
real, dimension(ims:ime,jms:jme)::t2,q2,psfc,t2_old,q2_old,psfc_old, &
   rainc,rainnc,rain_old
logical::initialize
integer::fmoist_init
real::moisture_dt

call init_fuel_cats(.true.)


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

end program moisture_main
