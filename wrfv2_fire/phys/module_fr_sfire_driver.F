!WRF:MEDIATION_LAYER:FIRE_MODEL

!
!*** Jan Mandel August-October 2007 
!*** email: jmandel@ucar.edu or Jan.Mandel@gmail.com or Jan.Mandel@cudenver.edu
!
! This file contains parts copied and/or adapted from earlier codes by
! Terry Clark, Janice Coen, Don Latham, and Net Patton.
!
! With contributions by Jonathan Beezley.



! This module is the only entry point from WRF-ARW to the wildland 
! fire model. The call to sfire_driver advances the fire model by 
! one timestep. The fire model inputs the wind and outputs 
! temperature and humidity tendencies. The fire model also inputs a 
! number of constant arrays (fuel data, topography). Additional 
! arguments are model state (for data assimilation) and constant arrays 
! the model gives to WRF for safekeeping because it is not allowed 
! to save anything.

! This model is described in [1]. The fire model is coupled with WRF 
! but the fire code itself is not dependent on WRF in any way other 
! than calls to few WRF utilities from module_fr_sfire_util. This 
! model uses a level set function method for advancing the fireline. 
! It is a reimplementation of an earlier model, which used fireline 
! propagation by tracers and was coupled with the Clark-Hall 
! atmospheric code, described in [2].

! Acknowledgements: Contributions to the level set method by Mijeong 
! Kim. The fire physics is adapted from an earlier code by Terry  
! L. Clark, Janice L. Coen, and Don Latham. The coupling with WRF is 
! adapted from a code by Ned Patton for coupling of the earlier fire
! model with WRF, with contributions by Jonathan D. Beezley. 

! [1] Jan Mandel, Jonathan D. Beezley, Janice L. Coen, and Minjeong Kim,
! Data Asimilation for Wildland Fires: Ensemble Kalman filters in 
! coupled atmosphere-surface models, IEEE Control Systems Magazine, 
! submitted, 2007

! [2] T. L. Clark, J. Coen, and D. Latham, Description of a coupled 
! atmosphere-fire model, Intl. J. Wildland Fire, vol. 13, pp. 49–64, 
! 2004


module module_fr_sfire_driver

use module_model_constants, only: cp,xlv
use module_fr_sfire_model
use module_fr_sfire_phys
use module_fr_sfire_util

logical write_m

contains

! module_fr_sfire_driver%%sfire_driver
subroutine sfire_driver (                         &
    ids,ide, kds,kde, jds,jde,                    & ! atm grid dimensions
    ims,ime, kms,kme, jms,jme,                    &
    ips,ipe, kps,kpe, jps,jpe,                    &
    ifds, ifde, jfds, jfde,                       & ! fire grid dimensions
    ifms, ifme, jfms, jfme,                       &
    igps, igpe, jgps, jgpe,                       & ! fire patch in - will use smaller
    ir,jr,                                        & ! atm/fire grid ratio
    num_tiles,i_start,i_end,j_start,j_end,        & ! atm grid tiling
    itimestep,ifuelread,nfuel_cat0,dt,dx,dy,      & ! in scalars
    u_frame,v_frame,                              &
    alfg,alfc,z1can,                              &
    u,v,mu,rho,zs,                                & ! in arrays, atm grid
    z_at_w,dz8w,                                  &
    lfn,tign,fuel_frac,                           & ! state arrays, fire grid
    rthfrten,rqvfrten,                            & ! out arrays, atm grid
    grnhfx,grnqfx,canhfx,canqfx,                  & ! out redundant arrays, atm grid  
    nfuel_cat,                                    & ! in array, data, fire grid, or constant internal
    fuel_time,zsf,                                & ! save constant internal data, fire grid
    dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip,ischap&
    )

implicit none

!*** arguments

integer, intent(in)::                             &
    ids,ide, kds,kde, jds,jde,                    & ! atm domain bounds
    ims,ime, kms,kme, jms,jme,                    & ! atm memory bounds 
    ips,ipe, kps,kpe, jps,jpe,                    & ! atm patch bounds
    ifds, ifde, jfds, jfde,                       & ! fire domain bounds
    ifms, ifme, jfms, jfme,                       & ! fire memory bounds
    igps, igpe, jgps, jgpe,                       & ! fire patch bounds given
    ir,jr,                                    & ! atm/fire grid refinement ratio
    itimestep,                                    & ! number of this timestep
    ifuelread,                                    & ! how to initialize nfuel_cat:
                                                       ! -1=not at all, done outside 
                                                       ! 0=from nfuel_cat0
                                                       ! 1=from altitude
                                                       ! 2=from file
    nfuel_cat0,                                   & ! fuel category to initialize everything to
    num_tiles                                       ! number of tiles

integer,dimension(num_tiles),intent(in) :: i_start,i_end,j_start,j_end  ! fire grid tiling

real, intent(in):: &
    dt,                                           & ! time step
    dx,dy,                                        & ! atm grid step
    u_frame,v_frame,                              & ! velocity offset
    z1can,                                        & ! lowest height crown fire heat is released (m)
    alfg,                                         & ! extinction depth of ground fire heat (m)
    alfc                                            !  extinction depth of crown fire heat (m)

real, intent(in), dimension(ims:ime, kms:kme, jms:jme):: &
    u,v,                                          & ! wind velocity (m/s) (node based, atm grid)
    mu,                                           & ! dry air mass (Pa)  pressure??  (cell based, atm grid)
    rho                                             ! air density  (kg/m^3) (cell based, atm grid)


real, intent(in), dimension(ims:ime, jms:jme)::   & ! from Ned's code, who knows that this means
    zs,                                           & ! terrain height  
    z_at_w,                                       & ! m abv sealvl
    dz8w                                            ! dz across w-lvl`
    
integer, intent(inout), dimension(ifms:ifme,jfms:jfme):: &
    nfuel_cat                                       ! fuel data; can be also set inside (cell based, fire grid)

real, intent(inout), dimension(ifms:ifme, jfms:jfme)::     &
    lfn,tign,fuel_frac                              ! state: level function, ign time, fuel left

real, intent(out), dimension(ims:ime, kms:kme, jms:jme):: &
    rthfrten,rqvfrten                              ! temperature and humidity tendencies (atm grid)

real, intent(out), dimension(ims:ime, jms:jme):: &  ! redundant arrays, for display purposes only (atm grid)
    grnhfx,                                      &  ! heat flux from ground fire (W/m^2) 
    grnqfx,                                      &  ! moisture flux from ground fire (W/m^2) 
    canhfx,                                      &  ! heat flux from crown fire (W/m^2) 
    canqfx                                         ! moisture flux from crown fire (W/m^2) 

!  ***** data (constant in time) *****

real, intent(inout), dimension(ifms:ifme, jfms:jfme):: &
    fuel_time,zsf,                               &
    dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip
integer, intent(inout), dimension(ifms:ifme, jfms:jfme):: ischap
    
!*** local
real :: dxf,dyf,time_start
integer,dimension(num_tiles) :: if_start,if_end,jf_start,jf_end, &  ! fire grid tiling
                                ia_start,ia_end,ja_start,ja_end     ! atm grid tiling
integer :: its,ite,jts,jte,kts,kte, &
    ij,i,j,k,initialize,ignition, &
    ifts,ifte,jfts,jfte,ifte1,jfte1, &           ! fire tile
    ifps, ifpe, jfps, jfpe, &                     ! fixed fire patch bounds
    iaps, iape, japs, jape                        ! fixed atm  patch bounds
real, dimension(ifms:ifme,jfms:jfme) :: uf,vf   ! fire wind velocities
real, dimension(ims:ime, jms:jme):: ua,va ! atm winds, averaged over height
character(len=128)msg
real, dimension(ifms:ifme, jfms:jfme):: fgrnhfx, fgrnqfx

!*** executable

! initialize and ignition should be really passed in arguments
! and set to 1 at the right time by the caller
if(itimestep .eq. 1 )then
    initialize=1
    ignition=1
else
    initialize=0
    ignition=0
endif

! time - assume dt does not change
time_start = itimestep * dt

! debug write switch
write_m = itimestep.le.2.or.mod(itimestep,100).eq.0

! fire mesh step
dxf=dx/ir
dyf=dy/ir

! fix fire patch to leave out one strip on domain boundary
! need fixed atm patch first, but that is never used except for checking 
! note the patch is the union of tiles 
iaps=max(ips,ids+1)
iape=min(ipe,ide-1)
japs=max(jps,jds+1)
jape=min(jpe,jde-1)
ifps= (iaps-ids)*ir+ifds    
ifpe= (iape-ids+1)*ir+ifds-1
jfps= (japs-jds)*jr+jfds    
jfpe= (jape-jds+1)*jr+jfds-1 

write(msg,'(a,i6,a,2(f15.6,a))')'time step',itimestep,' at',time_start,' duration',dt,'s'
call message(msg)
7001 format(a,' dimensions ',a4,':',i6,' to ',i6,' by ',i6,' to ',i6)
write(msg,'(a,2i9)')'refinement ratio:',ir,jr
call message(msg)
write(msg,'(a,2f15.6)')'atmosphere mesh step:',dx,dy
call message(msg)
write(msg,'(a,2f15.6)')'fire mesh step:      ',dxf,dyf
call message(msg)
write(msg,7001)'atm domain      ','ids',ids,ide,jds,jde
call message(msg)                    
write(msg,7001)'atm memory      ','ims',ims,ime,jms,jme
call message(msg)                    
write(msg,7001)'atm patch       ','ips',ips,ipe,jps,jpe
call message(msg)                    
write(msg,7001)'atm patch fixed ','iaps',iaps,iape,japs,jape
call message(msg)                    
write(msg,7001)'fire domain     ','ifds',ifds,ifde,jfds,jfde
write(msg,7001)'fire domain     ','ifds',ifds,ifde,jfds,jfde
call message(msg)                    
write(msg,7001)'fire memory     ','ifms',ifms,ifme,jfms,jfme
call message(msg)                    
write(msg,7001)'fire patch given','igps',igps,igpe,jgps,jgpe
call message(msg)                    
write(msg,7001)'fire patch fixed','ifps',ifps,ifpe,jfps,jfpe
call message(msg)                    

! check mesh dimensions
call check_fmesh(ids,ide,ifds,ifde,ir,'id')           ! check if atm and fire grids line up
call check_fmesh(jds,jde,jfds,jfde,jr,'jd')
call check_fmesh(iaps,iape,ifps,ifpe,ir,'ip')
call check_fmesh(japs,jape,jfps,jfpe,jr,'jp')
call check_mesh_2dim(ips,ipe,jps,jpe,ims,ime,jms,jme)        ! check if atm patch fits in atm array
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme) ! check if fire patch fits in fire array
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,igps,igpe,jgps,jgpe) ! check if fixed fire patch fits in given one 
call check_mesh_2dim(ips,ipe,jps,jpe,ids,ide,jds,jde)        ! check if atm patch fits in atm domain
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifds,ifde,jfds,jfde) ! check if fire patch fits in fire domain


! init rest of fuel tables with derived constants
call init_fuel_cats


!OMP PARALLEL DO PRIVATE(ij,i,j,k,its,ite,jts,jte,ifts,ifte,jfts,jfte,msg)
do ij=1,num_tiles

    ! set up tile bounds
    
    ! atm tiles leave out a strip on each side of domain
    ! fire tiles leave out a refined strip then
    its = max(i_start(ij),ids+1)  ! start atmospheric tile in i
    ite = min(i_end(ij),ide-1)    ! end atmospheric tile in i
    jts = max(j_start(ij),jds+1)  ! start atmospheric tile in j
    jte = min(j_end(ij),jde-1)    ! end atmospheric tile in j
    ifts= (its-ids)*ir+ifds       ! start fire tile in i
    ifte= (ite-ids+1)*ir+ifds-1   ! end fire tile in i
    jfts= (jts-jds)*jr+jfds       ! start fire tile in j
    jfte= (jte-jds+1)*jr+jfds-1   ! end fire tile in j
    
    ! store tile bounds for future loops
    
    ia_start(ij)=its              
    ia_end(ij)=ite
    ja_start(ij)=jts
    ja_end(ij)=jte
    if_start(ij)=ifts             
    if_end(ij)=ifte
    jf_start(ij)=jfts
    jf_end(ij)=jfte
    
    write(msg,*)'tile ',ij
    call message(msg)
    write(msg,7001)'atm tile   ','its',its,ite,jts,jte
    call message(msg)                   
    write(msg,7001)'fire tile  ','ifts',ifts,ifte,jfts,jfte
    call message(msg)                    

    ! check the tiles
    call check_mesh_2dim(its,ite,jts,jte,ips,ipe,jps,jpe)                 ! check if atm tile fits in atm patch
    call check_mesh_2dim(ifts,ifte,jfts,jfte,ifps,ifpe,jfps,jfpe)         ! check if fire tile fits in fire patch
    call check_mesh_2dim(its,ite+1,jts,jte+1,ims,ime,jms,jme)        ! check if atm node tile fits in memory 
    call check_mesh_2dim(ifts,ifte+1,jfts,jfte+1,ifms,ifme,jfms,jfme)! check if fire node tile fits in memory

    ! get atm winds at midpoints of sides
    k=kds             ! the ground
    do j = jts,jte
        do i = its,ite ! average 1st 2 layers, correct const shift
            ua(i,j)=0.5*( u(i,k,j) + u(i,k+1,j)) - u_frame
            va(i,j)=0.5*( v(i,k,j) + v(i,k+1,j)) - v_frame           
        enddo
    enddo

    if (write_m) then
        call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,ua,'ua',itimestep)
        call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,ua,'va',itimestep)
        call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,zs,'zs',itimestep)
    endif

enddo ! num_tiles

call print_2d_stats_vec(ids,ide,jds,jde,ims,ime,jms,jme,ua,va,'driver: atm wind (m/s)')

! The interpolation routine goes over each atm array tile
! and interpolate to the fire grid, and modify within the corresponding
! fire tile dimensions only. Since the fire grid tiles do not overlap
! this guarantees that two threads will not try to write to the same
! memory location, which might have unpredicable (bad) results.
!
! The relation between the atm and the fire grid is defined by the refinement ratio
! and a pair of nodes that line up. These nodes to not need to be within
! the present tile. These node pairs are different for the u and the v grid
! because on the atmospheric grid they are defined at different location.
!
! The fire model uses arrays based at corners of the cells. All arrays are cell centered.
! Each cell owns its lower left corner.
!
! The domain lower bounds are used to line up the atmospheric and the fire grids
! by the lower left corner of the domain.
! 
! The fire patch upper dimensions are used to decide if the interpolation should be done
! to fire nodes with index one higher than the tile dimension, this is the case when
! the tile is at the right edge or the top edge of the patch.
!
! For the interpolation to work properly the fire tiles that are adjacent to the
! domain boundary need to be smaller by one coarse atmospheric grid cell,
! because u and v do not carry enough information to interpolate there, and the 
! fire grid arrays would not be updated there completely anyway.
! In distributed parallel execution, on each patch the values of a  
! with offset on strip around the patch are also used and must be present.
!
! The same interpolation routine with different offsets is used:
!
! Interpolating u: atm grid shifted jr/2 up
! Interpolating v: atm grid shifted ir/2 right
! Interpolating z: atm grid shifted ir/2 right and jr/2 up
!
!                 F---v---F           
!                 |       |                  
!                 u   z   u 
!                 |       |              
!                 F---v---F                      
!                 |       |                  
!                 u   z   u 
!                 |       |              
!                 F---v---F                      
!
!                 F---v---F                   F---v---F                         
!                 |       |                   |       |                 
!                 u   z   u                   u   z   u
!                 |       |                   |       |        
!                 F---v---F                   F---v---F                     


!OMP PARALLEL DO PRIVATE(ij,i,j,its,ite,jts,jte,ifts,ifte,jfts,jfte,ifte1,jfte1)                  
do ij=1,num_tiles

    its=ia_start(ij)
    ite=ia_end(ij)
    jts=ja_start(ij)
    jte=ja_end(ij)
    ifts=if_start(ij)           ! fire tile
    ifte=if_end(ij)
    jfts=jf_start(ij)
    jfte=jf_end(ij)
    ifte1=endtilenode(ifte,ifpe) ! extend by one if last tile in patch
    jfte1=endtilenode(jfte,jfpe)

    ! interpolate winds from u,v on midpoints of cell faces to nodes on fire grid (u,v -> uf,vf)

    ! interpolate u velocity - staggered 1/2 atm grid cell up
    call interpolate_2d_nodes2nodes(  &
        its,ite,jts,jte,        & ! tile dims atm grid
        ims,ime,jms,jme,        & ! memory dims atm grid
        ifts,ifte1,jfts,jfte1,  & ! dimensions fire grid
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ir,jr,                  & ! refinement ratio
        ids,jds,ifds,jfds+jr/2, & ! line up by lower left corner of domain
        ua,                     & ! in atm grid     
        uf)                       ! out fire grid

    ! interpolate v velocity - staggered 1/2 atm grid cell right
    call interpolate_2d_nodes2nodes(  &
        its,ite,jts,jte,        & ! tile dims atm grid
        ims,ime,jms,jme,        & ! memory dims atm grid
        ifts,ifte1,jfts,jfte1,  & ! dimensions fire grid
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ir,jr,                  & ! refinement ratio
        ids,jds,ifds+ir/2,jfds, & ! line up by lower left corner of domain
        va,                     & ! in atm grid     
        vf)                       ! out fire grid

    ! interpolate terrain height zs -> zsf
    call interpolate_2d_nodes2nodes(  &
        its,ite,jts,jte,        & ! tile dims atm grid
        ims,ime,jms,jme,        & ! memory dims atm grid
        ifts,ifte1,jfts,jfte1,  & ! dimensions fire grid
        ifms,ifme,jfms,jfme,    & ! array dims fire grid
        ir,jr,                  & ! refinement ratio
        ids,jds,ifds+ir/2,jfds+jr/2, & ! line up by lower left corner of domain
        zs,                     & ! in atm grid     
        zsf)                      ! out fire grid
        
enddo
if(write_m)then
    call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,uf,'uf',itimestep)
    call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,vf,'vf',itimestep)
    call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,zsf,'zsf',itimestep)
endif
! the model is parallel itself, call on the whole patch not just a tile

call sfire_model (                    &
    initialize, ignition,                   & ! switches
    ifuelread,nfuel_cat0,                   & ! initialize fuel categories
    ifds,ifde,jfds,jfde,                    & ! fire domain dims
    ifms,ifme,jfms,jfme,                    & ! fire memory dims
    ifps,ifpe,jfps,jfpe,                    & ! fire patch dims
    num_tiles,if_start,if_end,jf_start,jf_end,  & ! fire grid tiling
    time_start,dt,                          & ! time and increment
    dxf,dyf,                                & ! fire mesh spacing
    zsf,                                    & ! terrain height (for gradient)
    uf,vf,                                  & ! input: wind
    lfn,tign,fuel_frac,                     & ! state: level function, ign time, fuel left
    fgrnhfx,fgrnqfx,                        & ! output: heat fluxes
    nfuel_cat,                              & ! fuel data per point 
    fuel_time,                              & ! save derived internal data
    dzfsdx,dzfsdy,bbb,betafl,phiwc,r_0,fgip,ischap &
    )


!OMP PARALLEL DO PRIVATE(ij,its,ite,jts,jte,ifts,ifte,jfts,jfte,msg)
do ij=1,num_tiles

    !get the tile
    its = ia_start(ij)             ! start atmospheric tile in i
    ite = ia_end(ij)               ! end atmospheric tile in i
    jts = ja_start(ij)             ! start atmospheric tile in j
    jte = ja_end(ij)               ! end atmospheric tile in j
    ifts= if_start(ij)             ! matching fire tile
    ifte= if_end(ij)
    jfts= jf_start(ij)
    jfte= jf_end(ij)

    ! we do not have canopy fluxes yet...
    do j=jts,jte
        do i=its,ite
            canhfx(i,j)=0
            canqfx(i,j)=0
        enddo
    enddo

    if(write_m)then
    call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,lfn,'lfn',itimestep)
    call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,tign,'tign',itimestep)
    call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fuel_frac,'fuel_frac',itimestep)
    call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnhfx,'fgrnhfx',itimestep)
    call write_array_m(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnqfx,'fgrnqfx',itimestep)
    endif
    
    ! sum up the fluxes over atm cells
    call sum_2d_cells(                                  &
        ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnhfx, &
        its,ite,jts,jte,ims, ime, jms, jme, grnhfx)
    call sum_2d_cells(                                  &
        ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnqfx, &
        its,ite,jts,jte,ims, ime, jms, jme, grnqfx)

    if(write_m)then
    call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,grnhfx,'grnhfx',itimestep)
    call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,grnqfx,'grnqfx',itimestep)
    endif

    ! --- add heat and moisture fluxes to tendency variables by postulated decay
    
    kts = kps
    kte = kpe

    call fire_tendency(                 &
        ids,ide, kds,kde, jds,jde,      & ! dimensions
        ims,ime, kms,kme, jms,jme,      &
        its,ite, kts,kte, jts,jte,      & ! ???
        grnhfx,grnqfx,canhfx,canqfx,        & ! fluxes on atm grid 
        alfg,alfc,z1can,                &
        zs,z_at_w,dz8w,mu,rho,          &
        rthfrten,rqvfrten)                ! out

    call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,rthfrten,'driver:rthfrten')
    call print_3d_stats(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,rqvfrten,'driver:rqvfrten')
    do k=kts,min(kts+4,kte)
        write(msg,'(a,i4)')'driver:rthfrten layer k=',k
        call print_3d_stats(its,ite,k,k,jts,jte,ims,ime,kms,kme,jms,jme,rthfrten,msg)
        write(msg,'(a,i4)')'driver:rqvfrten layer k=',k
        call print_3d_stats(its,ite,k,k,jts,jte,ims,ime,kms,kme,jms,jme,rqvfrten,msg)
    enddo

    if(write_m)then
    call write_array_m3(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,rthfrten,'rthfrten',itimestep)
    call write_array_m3(its,ite,kts,kte,jts,jte,ims,ime,kms,kme,jms,jme,rqvfrten,'rqvfrten',itimestep)
    endif

enddo ! tiles

end subroutine sfire_driver

!
!*****************************
!

subroutine check_fmesh(ids,ide,ifds,ifde,ir,s)
!*** purpose: check if fire and atm meshes line up
implicit none
!*** arguments
integer, intent(in)::ids,ide,ifds,ifde,ir
character(len=*),intent(in)::s
!*** local
character(len=128)msg
!*** executable
if ((ide-ids+1)*ir.ne.(ifde-ifds+1))then
    write(msg,1)s,ids,ide,ifds,ifde,ir
1   format('module_fr_sfire_driver: incompatible bounds ',a,' atm ',i5,':',i5,' fire ',i5,':',i5,' ratio ',i3)    
    call crash(msg)
endif
end subroutine check_fmesh

!
!*****************************
!

SUBROUTINE fire_tendency( &
    ids,ide, kds,kde, jds,jde,   & ! dimensions
    ims,ime, kms,kme, jms,jme,   &
    its,ite, kts,kte, jts,jte,   &
    grnhfx,grnqfx,canhfx,canqfx, & ! heat fluxes summed up to  atm grid 
    alfg,alfc,z1can,             & ! coeffients, properties, geometry 
    zs,z_at_w,dz8w,mu,rho,       &
    rthfrten,rqvfrten)             ! theta and Qv tendencies 

! This routine is atmospheric physics 
! it does NOT go into module_fr_sfire_phys because it is not fire physics

! taken from the code by Ned Patton, only order of arguments change to the convention here
! --- this routine takes fire generated heat and moisture fluxes and
!     calculates their influence on the theta and water vapor 
! --- note that these tendencies are valid at the Arakawa-A location

   IMPLICIT NONE

! --- incoming variables

   INTEGER , INTENT(in) :: ids,ide, kds,kde, jds,jde, &
                           ims,ime, kms,kme, jms,jme, &
                           its,ite, kts,kte, jts,jte

   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: grnhfx,grnqfx  ! W/m^2
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: canhfx,canqfx  ! W/m^2
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: zs  ! topography (m abv sealvl)
   REAL, INTENT(in), DIMENSION( ims:ime,jms:jme ) :: mu  ! dry air mass (Pa)

   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: z_at_w ! m abv sealvl
   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: dz8w   ! dz across w-lvl
   REAL, INTENT(in), DIMENSION( ims:ime,kms:kme,jms:jme ) :: rho    ! density

   REAL, INTENT(in) :: alfg ! extinction depth ground fire heat (m)
   REAL, INTENT(in) :: alfc ! extinction depth crown  fire heat (m)
   REAL, INTENT(in) :: z1can    ! height of crown fire heat release (m)

! --- outgoing variables

   REAL, INTENT(out), DIMENSION( ims:ime,kms:kme,jms:jme ) ::   &
       rthfrten, & ! theta tendency from fire (in mass units)
       rqvfrten    ! Qv tendency from fire (in mass units)
! --- local variables

   INTEGER :: i,j,k
   INTEGER :: i_st,i_en, j_st,j_en, k_st,k_en

   REAL :: cp_i
   REAL :: rho_i
   REAL :: xlv_i
   REAL :: z_w
   REAL :: fact_g, fact_c

   REAL, DIMENSION( ims:ime,kms:kme,jms:jme ) :: hfx,qfx

! --- set some local constants
   

   cp_i = 1./cp     ! inverse of specific heat
   xlv_i = 1./xlv   ! inverse of latent heat

! --- set loop indicies : note that 

   i_st = MAX(its,ids+1)
   i_en = MIN(ite,ide-1)
   k_st = kts
   k_en = MIN(kte,kde-1)
   j_st = MAX(jts,jds+1)
   j_en = MIN(jte,jde-1)

! --- distribute fluxes

   DO j = j_st,j_en
      DO k = k_st,k_en
         DO i = i_st,i_en

            ! --- set z (in meters above ground)

            z_w = z_at_w(i,k,j) - zs(i,j) ! should be zero when k=k_st

            ! --- heat flux

            fact_g = cp_i * EXP( - alfg * z_w )
            IF ( z_w < z1can ) THEN
               fact_c = cp_i
            ELSE
               fact_c = cp_i * EXP( - alfc * (z_w - z1can) )
            END IF
            hfx(i,k,j) = fact_g * grnhfx(i,j) + fact_c * canhfx(i,j) 

            ! --- vapor flux

            fact_g = xlv_i * EXP( - alfg * z_w )
            IF (z_w < z1can) THEN
               fact_c = xlv_i
            ELSE
               fact_c = xlv_i * EXP( - alfc * (z_w - z1can) )
            END IF
            qfx(i,k,j) = fact_g * grnqfx(i,j) + fact_c * canqfx(i,j) 

         END DO
      END DO
   END DO

! --- add flux divergence to tendencies
!
!   multiply by dry air mass (mu) to eliminate the need to 
!   call sr. calculate_phy_tend (in dyn_em/module_em.F)

   DO j = j_st,j_en
      DO k = k_st,k_en-1
         DO i = i_st,i_en

            rho_i = 1./rho(i,k,j)

            rthfrten(i,k,j) = - mu(i,j) * rho_i * (hfx(i,k+1,j)-hfx(i,k,j)) / dz8w(i,k,j)
            rqvfrten(i,k,j) = - mu(i,j) * rho_i * (qfx(i,k+1,j)-qfx(i,k,j)) / dz8w(i,k,j)

         END DO
      END DO
   END DO

   RETURN

END SUBROUTINE fire_tendency


end module module_fr_sfire_driver
