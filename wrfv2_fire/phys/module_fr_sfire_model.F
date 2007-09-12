module module_fr_sfire_model
!
!*** Jan Mandel September 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
contains

subroutine sfire_model &
                (ids,ide, kds,kde, jds,jde,                    & ! incoming
                 ims,ime, kms,kme, jms,jme,                    &
                 its,ite, kts,kte, jts,jte,                    &
                 ifds,ifde, kfds,kfde, jfds,jfde,              &
                 ifms,ifme, kfms,kfme, jfms,jfme,              &
                 nfrx,nfry,                                    &
                 itimestep,dt,dx,dy,                           &
                 u,v,                                          )  
                 grid_id,cen_lat,cen_lon,lat_ll,lon_ll,        &
                 moad_cen_lat,moad_cen_lon,                    &
                 moad_lat_ll,moad_lon_ll,moad_dx,moad_dy,      &
                 moad_s_we,moad_e_we,moad_s_sn,moad_e_sn,      &
                 tlat_stf,tlon_stf,t_ignite,ishape,ibeh,       &
                 z1can,alfg,alfc,ifuelread,nfuel_cat0,         &
                 z,z_at_w,dz8w,zs,u,v,mu,rho,                  &
                 nfuel_cat,nfl,nfl_t,nfl_c,ncod,               & ! in and out
                 in1,in2,ixb,iyb,icn,                          &
                 fg,fc,r_0,bbb,betafl,phiwc,area,area2,        &
                 zf,zsf,tign_g,tign_c,tign_crt,                &
                 xfg,yfg,xcd,ycd,xcn,ycn,sprdx,sprdy,          &
                 rthfrten,rqvfrten,                            & ! outgoing
                 grnhfx,grnqfx,canhfx,canqfx)  ! temp? 


use module_fr_sfire_core
use module_fr_sfire_util
use module_fr_sfire_speed

implicit none

!*** arguments

INTEGER, INTENT(in) :: ids,ide, kds,kde, jds,jde    ! atmosphere domain indices
INTEGER, INTENT(in) :: ims,ime, kms,kme, jms,jme    ! atmosphere memory indices
INTEGER, INTENT(in) :: its,ite, kts,kte, jts,jte    ! atmosphere tile   indices
 
INTEGER, INTENT(in) :: ifds,ifde, jfds,jfde  ! fire domain indices
INTEGER, INTENT(in) :: ifms,ifme, jfms,jfme  ! fire memory indices

INTEGER, INTENT(in) :: itimestep         ! current time step (cumulative)
REAL,    INTENT(in) :: dt                ! time step
REAL,    INTENT(in) :: dx,dy             ! dx,dy on innermost atm mesh

!  ***** data (constant in time) *****
! height above sea level of w points (m)
REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: z,z_at_w ! constant

REAL, INTENT(in), DIMENSION( ifms,ifme, jfms,jfme ):: &
           fgi !initial total mass of surface fuel (kg/m^2), at fire cells

!  ***** inputs *****
! incoming atmos. winds (m/s at arakawa-c grid locations)
REAL, INTENT(in), DIMENSION( ims:ime, kms:kme, jms:jme ) :: u,v

!  ***** state ***** 
real, intent(inout), dimension(ifms,ifme, jfms,jfme):: &
            phi,        & ! the level function, node based, fire is where phi<0
            tign,       & ! ignition time, node based          
            fuel_frac   & ! fuel fraction, cell based (dependent)
            
            
!*** local

integer::iats,iate,jats,jate         ! atm grid size
! fire grid size declared in module_fr_sfire_speed
real, dimension(ifms:ifme, jfms:jfme):: & 
    phi_start,      & ! the level function at start (of this call)
    fuel_time,      & ! time for fuel to burn down to 1/e original amount (data)
    fuel_frac_start,& ! the fuel fraction at start
    zf,zsf          & ! terrain height
    
! winds, kw defined in the speed function module
real, dimension(ifms:ifme, jfms:jfme,1:kw):: vfx,vfy
real, target, dimension(ifms:ifme, jfms:jfme)::t_vx,t_vy  ! the advection field for the speed function 
real:: fdx,fdy,time_start

!*** executable

! associate the data arrays with pointers in module_fr_sfire_speed, avoid prohibited heap storage
! for use in the speed function as arrays vx, vy
vx => t_vx
vy => t_vy

! time - assume dt does not change
time_start = itimestep * dt

! mesh step
fdx=dx/nfrx
fdy=dy/nfrx

! init rest of fuel tables with derived quantities
call init_fuel_cats

! get the index range of atm cells over the fire domain
! leave out one cell strip at domain boundary
iats = MAX( its, ids+1)
iate = MIN( ite, ide-1)
jats = MAX( jts, jds+1)
jate = MIN( jte, jde-1)

! get the index range of the matching cells on the refined fire grid
ifts = (iats-1)* nfrx + 1
ifte = iate * nfrx
jfts = (jats-1)* nfry + 1
jfte = jate * nfry

write(msg,7001)'atm domain ','ids',ids,ide,jds,jde
call message(msg)                    
write(msg,7001)'atm memory ','ims',ims,ime,jms,jme
call message(msg)                    
write(msg,7001)'atm patch  ','its',its,ite,jts,jte
call message(msg)                    
write(msg,7001)'fire domain','ifds',ifds,ifde,jfds,jfde
call message(msg)                    
write(msg,7001)'fire memory','ifms',ifms,ifme,jfms,jfme
call message(msg)                    
write(msg,7001)'atm used   ','iats',iats,iate,jats,jate
call message(msg)                   
write(msg,7001)'fire used  ','ifts',ifts,ifte,jfts,jfte
call message(msg)                    
7001 format(a,' dimensions ',a4,':',i8,' to ',i8,' by ',i8,' to ',i8)
write(msg,'(a,2i9)')'refinement ratio:',nfrx,nfry
call message(msg)

! check if nodal mesh fits into the declared memory 
check_mesh_2dim(iats,iate+1,jats,jate+1,ims,ime,jms,jme)
check_mesh_2dim(ifts,ifte+1,jfts,jfte+1,imfs,imfe,jmfs,jmfe)

! interpolate winds from u,v on the 3d arakawa c-grid (midpoints of faces)
! to vfx,vfy on nodes of the fire grid, in layers 1 to kw
! kw, vx,vy is declared in module_fr_sfire_speed
! *** this might be optimized away and the advection field computed from
! the atm c-grid points directly ***
call atm2fire_winds(                      &
    iats,iate,jats,jate,                &
    ims,ime, kms,kme,jms,jme,           & 
    ifts,ifte,jfts,jfte,1,kw,           &
    ifms,ifme,jfms,jfme,1,kw,           &
    u,v,vfx,vfy)

!interpolate topography from cell centers on atm grid to nodes on fire grid (zs -> zsf)
call interpolate_2d_cells2nodes(              &
    iats,iate,jats,jate, &
    ims, ime, jms, jme, z, &  ! in  
    ifts,ifte,jfts,jfte, &
    ifms,ifme,jfms,jfme,zf  )  ! out

!***** set up the rest of the data here:
! vx, vy - background advection field (winds) for prop_ls
! 

!???????????
      
! advance the model from time_start to time_start+dt
! will call module_fr_sfire_speed%%normal_speed for advection info 
subroutine sfire_core(     &
    ifts,ifte,jfts,jfte,                              &
    ifms,ifme,jfms,jfme,                              &
    time_start,time_diff,fire_dx,fire_dy,fuel_time,   &
    phi,tign,fuel_frac,fuel_frac_burnt ) 


! compute the heat fluxes on the fire grid cells
do j=jfts,jfte
    do i=ifts,ifte
         ff = frac_end(i,j)-frac_start(i,j)  !
    enddo
enddo

      


end subroutine sfire_model

end module module_fr_sfire_model
