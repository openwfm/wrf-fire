module module_domain
! define the main datastructure passed to the model
! follows WRFV3/frame/module_domain.F but only fields needed here are defined
   integer, parameter:: max_tiles=10

   type domain

      ! inputs to the fire model

      ! changes every time step
      real, pointer, dimension(:,:)::uf => null()              ! winds on the fire mesh 
      real, pointer, dimension(:,:)::vf => null()              ! winds on the fire mesh

      ! constant arrays - set once at initialization
      real, pointer, dimension(:,:)::zsf => null()             ! terrain height
      real, pointer, dimension(:,:)::dzdxf => null()           ! terrain gradient 
      real, pointer, dimension(:,:)::dzdyf => null()           ! terrain gradient 
      real, pointer, dimension(:,:)::fxlong=>null()            ! fire mesh nodes coordinates
      real, pointer, dimension(:,:)::fxlat =>null()            ! fire mesh nodes coordinates
      real, pointer, dimension(:,:)::nfuel_cat => null()       ! index to fuel data table
      ! nfuel_cat is expanded to the coefficient arrays below from the table in namelist.fire
      real, pointer, dimension(:,:)::bbb => null()             ! fire spread formula coefficients
      real, pointer, dimension(:,:)::betafl => null()
      real, pointer, dimension(:,:)::phiwc => null()
      real, pointer, dimension(:,:)::r_0 => null()
      real, pointer, dimension(:,:)::fgip  => null()
      real, pointer, dimension(:,:)::ischap => null()          ! fire spread formula coefficients
      real, pointer, dimension(:,:)::fuel_time => null()       ! fuel coefficient  

      ! mesh and housekeeping

      real:: sr_x, sr_y                                        ! atmosphere/fire refinement ratio
      integer::itimestep                                       ! time step number
      integer:: num_tiles                                      ! parallel execution tiles info
      integer, dimension(max_tiles):: i_start,i_end,j_start,j_end
      real:: dt,dx,dy                                          ! time and space stepsizes (atmosphere scale)
      real:: u_frame,v_frame                                   ! must be zero, for compatibility

      ! fire model state arrays 
       
      ! because of WRF architecture, the fire model may not maintain any state internally
      ! all state arrays must be passed in every call
      real, pointer, dimension(:,:)::lfn => null()             ! level set function
      real, pointer, dimension(:,:)::tign_g => null()          ! time of ignintion
      real, pointer, dimension(:,:)::fuel_frac => null()       ! remaining fuel fraction
      real, pointer, dimension(:,:)::lfn_out  => null()        ! a copy of the level set function
      real, pointer, dimension(:,:)::fire_area  => null()      ! fraction of cell area burning

      ! fire model outputs

      real, pointer, dimension(:,:)::fgrnhfx  => null()        ! ground sensible heat flux
      real, pointer, dimension(:,:)::fgrnqfx  => null()        ! ground latent heat flux
      real, pointer, dimension(:,:)::fcanhfx  => null()        ! canopy sensible heat flux
      real, pointer, dimension(:,:)::fcanqfx  => null()        ! canopy latent heat flux
      real, pointer, dimension(:,:)::ros  => null()            ! rate of spread - for diagnostics only

      ! atmospheric variables for compatibility

      real, pointer, dimension(:,:,:)::u_2 => null()           ! wind on atmospheric mesh
      real, pointer, dimension(:,:,:)::v_2 => null()           ! wind on atmospheric mesh
      real, pointer, dimension(:,:)::ht => null()              ! terrain height, on atmosphere mesh
      real, pointer, dimension(:,:)::xlong => null()           ! nodal coordinates
      real, pointer, dimension(:,:)::xlat => null()            ! nodal coordinates 
      real, pointer, dimension(:,:)::grnhfx => null()          ! outputs heat fluxes on atm grid 
      real, pointer, dimension(:,:)::grnqfx => null()
      real, pointer, dimension(:,:)::canhfx => null()
      real, pointer, dimension(:,:)::canqfx => null()           

   end type domain

end module module_domain
