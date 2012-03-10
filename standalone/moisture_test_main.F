! standalone moisture model test
! inpout from file moisture_input.txt with each line of the form

! Time  T P  Q RAIN
! h     K Pa 1 mm/h

program moisture_main
use module_fr_sfire_phys, only: advance_moisture, init_fuel_cats, moisture_classes
use module_fr_sfire_util, only: fire_print_msg 
implicit none

character(len=128)::infile,outfile,prtfile
integer, parameter::nfmc=5 ! change if needed

integer, parameter:: ims=1,ime=10,jms=1,jme=5
integer::its=ims,ite=ime,jts=jms,jte=jme
real, dimension(ims:ime,1:nfmc,jms:jme)::fmc_gc,fmc_equi,fmc_tend
real, dimension(ims:ime,jms:jme)::t2,q2,psfc,t2_old,q2_old,psfc_old, &
   rainc,rainnc,rain_old
logical::initialize
integer::fmoist_init=2   ! initialize at equilibrium
real::moisture_dt,dt
integer::istep,i,k,nsteps=999999
real::t,p,q,rain,hours,fmc(nfmc),hours_old

!*** executable
fire_print_msg = 0   ! change to 2 to get more debugging prints

infile='moisture_input.txt'
outfile='moisture_output.txt'
prtfile='moisture_print.txt'

call init_fuel_cats(.true.)
print *,moisture_classes,' moisture classes'

open(1,file=trim(infile),form='formatted',status='old',err=91)
open(7,file=trim(prtfile),form='formatted',status='unknown',err=93)
open(8,file=trim(outfile),form='formatted',status='unknown',err=92)

! write header
do i=6,7
    write(i,1)'Time', 'T','P','Q','RAIN',('FMC',k,k=1,moisture_classes)
    write(i,2)'hours','K','Pa','kg/kg','mm',('kg/kg',k=1,moisture_classes)
enddo
1 format(a8,  a6,  a9,  a6  ,a8, 9(a5,i1))
2 format(a8,  a6,  a9,  a6  ,a8, 9a6)
3 format(f8.2,f6.1,f9.1,f6.3,f8.1,9f6.3)

do istep=1,999999

    ! read data line
    read(1,*,end=9,err=94)hours,t,p,q,rain

    ! set the values in

    if(istep.eq.1)then
        moisture_dt=0.  ! required on initialization
        initialize=.true.
        hours_old=hours
    else
        moisture_dt=3600*(hours - hours_old)  !
        initialize=.false.
    endif

    t2=t
    q2=q
    psfc=p
    rainc=0.
    rainnc=rain
       
    write(6,*) 'step ',istep

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

    do k=1,moisture_classes
        write(6,*)'Class ',k, 'FMC equilibrium ',fmc_equi(its,k,jts),' tendency ',fmc_tend(its,k,jts)
    enddo

    ! print the output
    do i=6,8
        write(i,3)hours,t,p,q,rain,(fmc_gc(its,k,jts),k=1,moisture_classes)
    enddo

enddo

write(6,*)'Maximum number of steps reached before reading whole input file ',trim(infile)

9 continue
write(6,*)'Completed ',istep,' steps'
write(6,*)'Input from flat text file ',trim(infile) 
write(6,*)'Output with headers is in ',trim(prtfile) 
write(6,*)'Output as a flat text is in ',trim(outfile) 

close(7)
close(8)

goto 999

91 print *,'Cannot open ',trim(infile)
goto 998
92 print *,'Cannot open ',trim(outfile)
goto 998
93 print *,'Cannot open ',trim(prtfile)
goto 998
94 print *,'Error reading record ',istep,' in file ',trim(infile)

998 continue

999 continue

end program moisture_main
