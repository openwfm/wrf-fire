!
!*** Jan Mandel October 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!

module module_fr_sfire_model

use module_fr_sfire_core
use module_fr_sfire_util
use module_fr_sfire_phys

contains

subroutine sfire_model (                    &
    id,                                     &
    ifun,                                   & ! switches
    initialize,                             & ! 
    num_ignitions,                          & ! number of ignitions before advancing
    ifuelread,nfuel_cat0,                   & ! initialize fuel categories
    ifds,ifde,jfds,jfde,                    & ! fire domain dims - the whole domain
    ifms,ifme,jfms,jfme,                    & ! fire memory dims - how declared
    ifts,ifte,jfts,jfte,                    & ! fire patch dims  - this processor
    time_start,dt,                          & ! time and increment
    fdx,fdy,                                & ! fire mesh spacing
    ignition_start_x,ignition_start_y,      & ! ignition - small arrays
    ignition_end_x,ignition_end_y,          &
    ignition_radius,                        &
    ignition_time,                          &
    zsf,                                    & ! terrain height (for gradient)
    vx,vy,                                  & ! input: wind
    lfn,tign,fuel_frac,                     & ! state: level function, ign time, fuel left
    grnhfx,grnqfx,                          & ! output: heat fluxes
    nfuel_cat,                              & ! fuel data per point 
    fuel_time,                              & ! save derived internal data
    dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip,ischap &
) 

! This subroutine implements the fire spread model.
! All quantities are on the fire grid. It inputs
! winds given on the nodes of the fire grid
! and outputs the heat fluxes on the cells of the fire grid.
! This subroutine has no knowledge of any atmospheric model.
!
! All computation is done on the tile. 

implicit none

!*** arguments

! input
integer, intent(in) :: id
integer, intent(in) :: ifun
integer, intent(in) :: initialize           ! 0 = initialize simulation
                                            ! 1 = initialize timestep (in
integer, intent(in) :: num_ignitions        ! number of ignition locations/times 
integer, intent(in) :: ifuelread,nfuel_cat0 ! for set_fire_params
integer, intent(in) :: ifds,ifde,jfds,jfde  ! fire domain bounds
integer, intent(in) :: ifts,ifte,jfts,jfte  ! fire tile bounds
integer, intent(in) :: ifms,ifme,jfms,jfme  ! fire memory array bounds
REAL,INTENT(in) :: time_start,dt            ! starting time, time step
REAL,INTENT(in) :: fdx,fdy                  ! spacing of the fire mesh
real, dimension(num_ignitions), intent(in):: &   
    ignition_start_x,ignition_start_y, &
    ignition_end_x,ignition_end_y,ignition_radius, & ! start, end, radius, time
    ignition_time                           !  of ignition lines
REAL, INTENT(in), dimension(ifms:ifme,jfms:jfme):: & 
    zsf,   &                                ! terrain height, node based, data, constant        
    vx,vy                                   ! wind m/s (node based), data, variable
    
! state
REAL, INTENT(inout), dimension(ifms:ifme,jfms:jfme):: &
    lfn   , &                               ! level function: fire is where lfn<0 (node)
    tign  , &                               ! absolute time of ignition (node)
    fuel_frac                               ! fuel fraction (node), currently redundant
    
! output
REAL, INTENT(out), dimension(ifms:ifme,jfms:jfme):: &
    grnhfx,grnqfx                           ! heat fluxes J/m^2/s  (cell)             
 
! constant arrays - set at initialization
integer, intent(inout), dimension(ifms:ifme, jfms:jfme)::nfuel_cat ! cell based, data, constant
real,intent(inout),dimension(ifms:ifme,jfms:jfme):: fuel_time
real,intent(inout),dimension(ifms:ifme,jfms:jfme):: dzfsdx,dzfsdy        ! (node) terrain gradient (1)
real,intent(inout),dimension(ifms:ifme,jfms:jfme):: bbb,betafl,phiwc,r_0 ! (node) spread formula coefficients
real,intent(inout),dimension(ifms:ifme,jfms:jfme):: fgip                 ! (cell) init mass of surface fuel (kg/m^2)
integer,intent(inout),dimension(ifms:ifme,jfms:jfme):: ischap            ! (node) .ne.0 if chapparal

!*** local

integer :: xifms,xifme,xjfms,xjfme  ! memory bounds for pass-through arguments to normal spread
integer::ignited,ig,i,j
real, dimension(ifts:ifte,jfts:jfte)::fuel_frac_burnt,fuel_frac_start
real,dimension(ifms:ifme,jfms:jfme):: lfn_out
real::tbound

!*** executable

xifms=ifms  ! dimensions for the include file
xifme=ifme
xjfms=jfms
xjfme=jfme

call print_2d_stats_vec(ifts,ifte+1,jfts,jfte+1,ifms,ifme,jfms,jfme,vx,vy,'model: fire wind (m/s)')


!if(ifun.eq.1)then  ! check for init and ignition

    if (initialize.ne.0) then
                    
        ! initialize all arrays that the model will not change later
        call set_nfuel_cat( &
                            ifms,ifme,jfms,jfme, &
                            ifts,ifte,jfts,jfte, &
                            ifuelread,nfuel_cat0,&
                            zsf,nfuel_cat)

        ! set_fire_params treats nfuel_cat as data
        call set_fire_params(   & 
                                ifms,ifme,jfms,jfme, &
                                ifts,ifte,jfts,ifte, &
                                fdx,fdy,             &
                                zsf,nfuel_cat,fuel_time &
#                               include "fr_sfire_params_args.h" 
)
    
        ! initialize variable model arrays to no fire
        call init_no_fire   ( &
            ifds,ifde,jfds,jfde, &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            fdx,fdy,time_start,  &
            fuel_frac,lfn,tign) 
                            
    endif

    do ig = 1,num_ignitions
        if(ignition_time(ig)>=time_start.and.ignition_time(ig)<time_start+dt)then 
            call ignite_fire(                             &
                ifds,ifde,jfds,jfde,                      & ! fire domain dims - the whole domain
                ifms,ifme,jfms,jfme,                      &
                ifts,ifte,jfts,jfte,                      &
                ignition_start_x(ig),ignition_start_y(ig),&
                ignition_end_x(ig),ignition_end_y(ig),    &
                ignition_radius(ig),                      &
                ignition_time(ig),                        &  
                fdx,fdy,                                  &
                lfn,tign,ignited)
        endif
    enddo
    
!elseif (ifun.eq.2) then

! advance the model from time_start to time_start+dt
! return the fuel fraction burnt this call in each fire cell
! will call module_fr_sfire_speed::normal_spread for propagation speed
! We cannot simply compute the spread rate here because that will change with the
! angle of the wind and the direction of propagation, thus it is done in subroutine
! normal_spread at each fire time step. Instead, we pass arguments that 
! subroutine normal_spread may use. The include is to guarantee this is done consistently
! over the call chain.

!   compute fuel fraction at start
    call get_fuel_left( &
        ifms,ifme,jfms,jfme, &
        ifts,ifte,jfts,jfte, &
        fdx,fdy,                        &
        lfn,tign,fuel_time,time_start,fuel_frac_start) ! 

!   do time step
    call prop_ls(id,     &
    ifds,ifde,jfds,jfde,                      & ! fire domain dims - the whole domain
    ifms,ifme,jfms,jfme,                      &
    ifts,ifte,jfts,jfte,                      &
    time_start,dt,fdx,fdy,tbound,  &
    lfn,lfn_out,tign &
#   include "fr_sfire_params_args.h" 
    ) 
    lfn=lfn_out

!elseif (ifun.eq.3) then
! compute the heat fluxes from the fuel burned

    call get_fuel_left( &
        ifms,ifme,jfms,jfme, &
        ifts,ifte,jfts,jfte, &
        fdx,fdy,                        &
        lfn,tign,fuel_time,time_start+dt,fuel_frac) ! replace by time_now later
    
    do j=jfts,jfte
        do i=ifts,ifte
            fuel_frac_burnt(i,j)=fuel_frac_start(i,j)-fuel_frac(i,j)
        enddo
    enddo
        
    call heat_fluxes(dt,                          &
        ifts,ifte,jfts,jfte,                      &
        ifms,ifme,jfms,jfme,                      &
        fgip,fuel_frac_burnt,                     & !in
        grnhfx,grnqfx)                              !out

call print_2d_stats(ifts,ifte,jfts,jfte, &
                   ifms,ifme,jfms,jfme, &
                   grnhfx,'model: heat flux(J/m^2/s)')

end subroutine sfire_model

!
!*****************
!
            
end module module_fr_sfire_model
