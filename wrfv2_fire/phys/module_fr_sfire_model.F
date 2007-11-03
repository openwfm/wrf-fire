! WRF MEDIATION LAYER: PHYSICS
!
!*** Jan Mandel October 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!

module module_fr_sfire_model
contains

subroutine sfire_model (                    &
    initialize, ignition,                   & ! switches
    ifds,ifde,jfds,jfde,                    & ! fire domain dims - the whole domain
    ifms,ifme,jfms,jfme,                    & ! fire memory dims - how declared
    ifps,ifpe,jfps,jfpe,                    & ! fire patch dims  - this processor
    num_tiles,i_start,i_end,j_start,j_end,  & ! fire grid tiling - divide for openmp
    time_start,dt,                          & ! time and increment
    fdx,fdy,                                & ! fire mesh spacing
    nfuel_cat,                              & ! fuel data per point 
    zsf,                                    & ! terrain height (for gradient)
    vx,vy,                                  & ! input: wind
    lfn,tign,fuel_frac,                     & ! state: level function, ign time, fuel left
    grnhfx,grnqfx,                          & ! output: heat fluxes
    fuel_time,                              & ! save derived internal data
    dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip,ischap &
) 

! This subroutine implements the fire spread model.
! All quantities are on the fire grid. It inputs
! winds given on the nodes of the fire grid
! and outputs the heat fluxes on the cells of the fire grid.
! This subroutine has no knowledge of any atmospheric model.

use module_fr_sfire_core
use module_fr_sfire_util
use module_fr_sfire_phys
implicit none

!*** arguments

! input

integer, intent(in) :: initialize, ignition ! if not 0, do this and proceed with timestep
integer, intent(in) :: ifds,ifde,jfds,jfde  ! fire domain bounds
integer, intent(in) :: ifps,ifpe,jfps,jfpe  ! fire patch bounds
integer, intent(in) :: ifms,ifme,jfms,jfme  ! fire memory array bounds
integer,intent(in)::num_tiles
integer,intent(in),dimension(num_tiles)::i_start,i_end,j_start,j_end
REAL,INTENT(in) :: time_start,dt            ! starting time, time step
REAL,INTENT(in) :: fdx,fdy                  ! spacing of the fire mesh
integer, intent(in), dimension(ifms:ifme, jfms:jfme)::nfuel_cat ! cell based, data, constant
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
real,intent(inout),dimension(ifms:ifme,jfms:jfme):: fuel_time
real,intent(inout),dimension(ifms:ifme,jfms:jfme):: dzfsdx,dzfsdy        ! (node) terrain gradient (1)
real,intent(inout),dimension(ifms:ifme,jfms:jfme):: bbb,betafl,phiwc,r_0 ! (node) spread formula coefficients
real,intent(inout),dimension(ifms:ifme,jfms:jfme):: fgip                 ! (cell) init mass of surface fuel (kg/m^2)
integer,intent(inout),dimension(ifms:ifme,jfms:jfme):: ischap            ! (node) .ne.0 if chapparal

!*** local

INTEGER :: ifts,ifte,jfts,jfte      ! fire tile bounds
integer :: xifms,xifme,xjfms,xjfme  ! memory bounds for pass-through arguments to normal spread
integer::ij        
real, dimension(ifms:ifme, jfms:jfme)::fuel_frac_burnt
real:: ctri,ctrj,diam ! for ignition
character(len=128)msg

!*** executable

xifms=ifms  ! dimensions for the include file
xifme=ifme
xjfms=jfms
xjfme=jfme

do ij=1,num_tiles
    ifts=i_start(ij)
    ifte=i_end(ij)
    jfts=j_start(ij)
    jfte=j_end(ij)
    
    call check_tiles(ifts,ifte,jfts,ifte,num_tiles,i_start,i_end,j_start,j_end)
    
    write(msg,'(a,i5,a,f8.2)')'model: fire tile',ij,' avg fire wind', &
        avg_2darray_vec(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,vx,vy)
    call message(msg)
    
    ! initialize all arrays that the model will not change later
    call set_fire_params(   ifts,ifte,jfts,ifte, &
                            ifms,ifme,jfms,jfme, &
                            fdx,fdy,             &
                            zsf,nfuel_cat,fuel_time &
#                           include "fr_sfire_params_args.h" 
)
    
    if (initialize.ne.0) then
                    
        ! initialize variable model arrays to no fire
        call init_model_no_fire(ifts,ifte,jfts,ifte, &
                                ifms,ifme,jfms,jfme, &
                                fuel_frac,lfn,tign)                            
                            
    endif

    select case(ignition)
    case(0)
    case(1)
        ! ignite fire in the middle
        ctri=(ifds+ifde+1)*0.5
        ctrj=(jfds+jfde+1)*0.5
        ! at least 6 by 6 cells but no less than 5 m
        diam = max(5.0,6*max(fdx,fdy))    
        call ignite_fire(   ifts,ifte,jfts,jfte,                      &
                            ifms,ifme,jfms,jfme,                      &
                            time_start,ctri,ctrj,diam,fdx,fdy,lfn,tign)
        ! more complicated ignition can come here, or load from data
    case default
        call crash('model: unsupported ignition value')
    end select
    
enddo ! end tile loop

! advance the model from time_start to time_start+dt
! return the fuel fraction burnt this call in each fire cell
! will call module_fr_sfire_speed::normal_spread for propagation speed
! We cannot simply compute the spread rate here because that will change with the
! angle of the wind and the direction of propagation, thus it is done in subroutine
! normal_spread at each fire time step. Instead, we pass arguments that 
! subroutine normal_spread may use. The include is to guarantee this is done consistently
! over the call chain.

call sfire_core(     &
    ifts,ifte,jfts,jfte,                      &
    ifms,ifme,jfms,jfme,                      &
    num_tiles,i_start,i_end,j_start,j_end, & ! tiling
    time_start,dt,fdx,fdy,fuel_time,  &
    lfn,tign,fuel_frac,fuel_frac_burnt &
#   include "fr_sfire_params_args.h" 
    ) 

! compute the heat fluxes from the fuel burned
do ij=1,num_tiles
    ifts=i_start(ij)
    ifte=i_end(ij)
    jfts=j_start(ij)
    jfte=j_end(ij)
    
    call heat_fluxes(dt,                          &
        ifts,ifte,jfts,jfte,                      &
        ifms,ifme,jfms,jfme,                      &
        fgip,fuel_frac_burnt,                     & !in
        grnhfx,grnqfx)                              !out

enddo ! tiles

end subroutine sfire_model

!
!*****************
!
            
end module module_fr_sfire_model
