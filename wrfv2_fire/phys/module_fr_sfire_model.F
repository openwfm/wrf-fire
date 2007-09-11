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

integer::iats,iate,jats,jate         % atm grid size
! fire grid size declared in module_fr_sfire_speed
real, dimension(ifms:ifme, jfms:jfme):: & 
    phi_start,      & ! the level function at start (of this call)
    fuel_time,      & ! time for fuel to burn down to 1/e original amount (data)
    fuel_frac_start,& ! the fuel fraction at start
    
integer::i,j,k
real, dimension(ims:ime,jms:jme)::vxatm,vyatm
! winds, kw defined in the speed function module
real, target, dimension(ifms:ifme, jfms:jfme,kw):: t_vx,t_vy 
real:: fdx,fdy,time_start,err

!*** executable

! associate the data arrays with pointers in module_fr_sfire_speed
! for use in the speed function
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
check_mesh_dim(iats,iate+1,jats,jate+1,ims,ime,jms,jme)
check_mesh_dim(ifts,ifte+1,jfts,jfte+1,imfs,imfe,jmfs,jmfe)

! interpolate winds from the arakawa c-grid (midpoints of faces)
! to nodes of atm grid (cube corners) and then the fire grid
! vxatm and vyatm are the velocities at the upper South-East corner of cube (i,j,k)
! u(i,k,j) is the x velocity at the smallest x face of the cube cell (i,j,k)
! v(i,k,j) is the y velocity at the smallest y face of the cube cell (i,j,k)                                            
! kw=the number of wind layers is defined in module_fr_sfire_speed
DO k = 1,kw
    DO j = jats,jate+1  ! +1 to get winds on the high x and high y nodes 
        DO i = iats,iate+1
             vxatm(i,j) = .25*( u(i,k,j) + u(i,k,j-1) + v(i,k+1,j) + v(i,k+1,j-1) )
             vyatm(i,j) = .25*( v(i-1,k,j) + v(i,k,j) + v(i-1,k+1,j) + v(i,k+1,j) )
        END DO
    END DO
    call interpolate_2d_nodes2nodes(              &
      iats,iate+1,jats,jate+1,ims,ime,jms,jme,vxatm, &  ! in  
      ifts,ifte+1,jfts,jfte+1,ifms,ifme,jfms,jfme,vx(:,:,k ))  ! out
    call interpolate_2d_nodes2nodes(              &
      iats,iate+1,jats,jate+1,ims,ime,jms,jme,vyatm, &  ! in  
      ifts,ifte+1,jfts,jfte+1,ifms,ifme,jfms,jfme,vy(:,:,k ))  ! out
END DO
      
! set up the rest of the data
      

! save the level function for interpolation between the old and the new one later
do i=ifts,ifte+1
    do j=jfts,jfte+1
        phi_start(i,j)=phi(i,j)
    enddo
enddo 

! compute the fuel fraction at start
! this should not have to be done every timestep
! should be same as saved
! but needs to be redone at initialization
call fuel_left(ifts,ifte+1,jfts,jfte+1,ifms,ifme,jfms,jfme, &
    phi,tign,fuel_time,time,fuel_frac_start)

err=0
do i=ifts,ifte ! fuel fraction is cell based
    do j=jfts,jfte
        err=max(err,abs(fuel_frac_start(i,j)-fuel_frac(i,j)))
    enddo
enddo 
! will need reduction for MPI
write(msg,*)'fuel fraction error:',err    

! advance the level function from time_start to time_end
! prop_ls will call module_fr_sfire_speed%%normal_speed for advection info 

call prop_ls &
  (ids,ide,jds,jde,ids,ide,jds,jde,phi,time_start,time_end,fdx,fdy)

! set ignition time at the nodes the fireline moved over
do j=jfts,jfte+1
    do i=ifts,ifte+1
        if(phi_start(i,j)>0 .and. .not. phi(i,j)>0)then
            ! node was not burning at start but it is burning at end
            ! interpolate from the level functions at start and at end
            ! phi_start(i,j) is the level function value at time_start
            ! phi(i,j)       is the level function value at time_end
            ! 0              is the level function value at tign(i,j)
            tign(i,j)=time_start   &
                      + dt * phi_start(i,j) / (phi_start(i,j) - phi(i,j))
        endif
    enddo
enddo 

! compute the fuel fraction at time_end 

call fuel_left(ifts,ifte+1,jfts,jfte+1,ifms,ifme,jfms,jfme, &
    phi,tign,fuel_time,time,fuel_frac_start)

! compute the heat fluxes on the fire grid
do j=jfts,jfte
    do i=ifts,ifte
         ff = frac_end(i,j)-frac_start(i,j)  !
    enddo
enddo

      


end subroutine sfire_model

end module module_fr_sfire_model
