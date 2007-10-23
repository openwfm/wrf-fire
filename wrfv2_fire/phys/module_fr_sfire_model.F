module module_fr_sfire_model
!
!*** Jan Mandel September 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
contains

! entry points

! subroutine sfire_model

subroutine sfire_model (initialize, ignition,       &
                 ifds,ifde,jfds,jfde,               & ! domain dims
                 ifms,ifme,jfms,jfme,               & ! memory dims
                 num_tiles,i_start,i_end,j_start,j_end, & ! tiling
                 kw,                                &
                 time_start,dt,                     &
                 fdx,fdy,                           &
                 nfuel_cat,fuel_time,zsf,vfx,vfy,   &
                 lfn,tign,fuel_frac,                &
                 grnhft,grnqft                      &
#                include "fr_sfire_params_args.inc" 
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
INTEGER, INTENT(in) :: ifds,ifde,jfds,jfde  ! fire domain bounds
INTEGER, INTENT(in) :: ifms,ifme,jfms,jfme  ! fire memory array bounds
integer,intent(in)::num_tiles
integer,intent(in),dimension(num_tiles)::i_start,i_end,j_start,j_end
INTEGER, INTENT(in) :: kw                   ! number of vertical layers of wind (1=surface)
REAL,INTENT(in) :: time_start,dt            ! starting time, time step
REAL,INTENT(in) :: fdx,fdy                  ! spacing of the fire mesh
integer, intent(inout), dimension(ifms:ifme, jfms:jfme)::nfuel_cat
REAL, INTENT(inout), dimension(ifms:ifme,jfms:jfme)::fuel_time
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
    
! passed to subroutine normal_spread
#include "fr_sfire_params_decl.inc" 
    
            
!*** local
INTEGER :: ifts,ifte,jfts,jfte  ! fire tile bounds

integer::i,j,ij        
! fire grid size declared in module_fr_sfire_speed
real, dimension(ifms:ifme, jfms:jfme)::    fuel_frac_burnt
real:: dmass,ctri,ctrj,diam
integer, parameter:: printl=1
character(len=128)msg

!*** executable


do ij=1,num_tiles
    ifts=i_start(ij)
    ifte=i_end(ij)
    jfts=j_start(ij)
    jfte=j_end(ij)
    
    call check_tiles(ifts,ifte,jfts,ifte,num_tiles,i_start,i_end,j_start,j_end)
    
    ! initialize all arrays that the model will not change later
    call set_fire_params(   ifts,ifte,jfts,ifte, &
                            ifms,ifme,jfms,jfme, &
                            fdx,fdy,             &
                            zsf,nfuel_cat,fuel_time &
#                           include "fr_sfire_params_args.inc" 
)
    
    if (initialize.ne.0) then
                    
        ! initialize variable model arrays to no fire
        call init_model_no_fire(ifts,ifte,jfts,ifte, &
                                ifms,ifme,jfms,jfme, &
                                fuel_frac,lfn,tign)                            
                            
    endif

    if (ignition.ne.0) then
        ! fire in the middle
        ctri=(ifds+ifde+1)*0.5
        ctrj=(jfds+jfde+1)*0.5
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

    do j=jfts,jfte+1
        do i=ifts,ifte+1
            vx(i,j)=vfx(i,j,1)
            vy(i,j)=vfy(i,j,1)
        enddo
    enddo
    
enddo ! end tile loop

! advance the model from time_start to time_start+dt
! return the fuel fraction burnt this call in each fire cell
! will call module_fr_sfire_speed%%normal_speed for propagation speed
! uses vx,vy,r as a background propagation field 
call sfire_core(     &
    ifts,ifte,jfts,jfte,                      &
    ifms,ifme,jfms,jfme,                      &
    num_tiles,i_start,i_end,j_start,j_end, & ! tiling
    time_start,dt,fdx,fdy,fuel_time,  &
    lfn,tign,fuel_frac,fuel_frac_burnt &
#   include "fr_sfire_params_args.inc" 
    ) 


do ij=1,num_tiles
    ifts=i_start(ij)
    ifte=i_end(ij)
    jfts=j_start(ij)
    jfte=j_end(ij)
    
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

enddo ! tiles

end subroutine sfire_model

!
!*****************
!
            
end module module_fr_sfire_model
