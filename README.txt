! SFIRE - Spread fire model in WRF-Fire
!
!*** Jan Mandel August 2007 - March 2011 
!*** email: Jan.Mandel@gmail.com

! For support please subscribe to the wrf-fire mailing list at NCAR at
! http://mailman.ucar.edu/mailman/listinfo/wrf-fire
! or go to http://www.openwfm.org/wiki/WRF-Fire_user_support 

! Current drafts of the technical documentation and
! user's guide can be found at

! http://www.openwfm.org/wiki/WRF-Fire_documentation
! http://www.openwfm.org/wiki/WRF-Fire_publications

! This module is the only entry point from WRF-ARW to the wildland 
! fire model. The call to sfire_driver advances the fire model by 
! one timestep. The fire model inputs the wind and outputs 
! temperature and humidity tendencies. The fire model also inputs a 
! number of constant arrays (fuel data, topography). Additional 
! arguments are model state (for data assimilation) and constant arrays 
! the model gives to WRF for safekeeping because it is not allowed 
! to save anything.

! This code as of mid-2011 is described in [1]. If you use this code, 
! please acknowledge our work by citing [1].
! Thank you.

! Acknowledgements
!
! The fire physics code is adapted from the CAWFE code [2].
! The coupling with WRF is adapted from a code by Ned Patton, 
! coupling a Fortran 90 port of the CAWFE fire module to WRF [3].
! Support of refined fire grids in WRF was provided by John Michalakes.
! Jonathan D. Beezley has set up and maintained the WRF build and
! execution environment, provided software engineering infrastructure 
! including synchronization with the WRF repository, and was responsibe
! for all aspects of WRF modification. UCD students Minjeong Kim and
! Volodymyr Kondratenko have contributed to the implementation of the
! fire propagation by the level set method.

! Refefences
!
! [1] Jan Mandel, Jonathan D. Beezley, and Adam K. Kochanski, "Coupled
! atmosphere-wildland fire modeling with WRF 3.3 and SFIRE 2011, 
! Geoscientific Model Development (GMD) 4, 591-610, 2011. 
! doi:10.5194/gmd-4-591-2011
!
! [2] T. L. Clark, J. Coen, and D. Latham, Description of a coupled 
! atmosphere-fire model, Intl. J. Wildland Fire, vol. 13, pp. 49-64, 
! 2004
!
! [3] Edward G. Patton and Janice L. Coen, WRF-Fire: A Coupled 
! Atmosphere-Fire Module for WRF, Preprints of Joint MM5/Weather 
! Research and Forecasting Model Users' Workshop, Boulder, CO, 
! June 22-25, 2004, pp. 221-223, NCAR
!
! --------------------------------------------------------------------
! 
! CURRENT ACTIVITY
! 
! For current activity and development trends please check out
! http://ccm.ucdenver.edu/wiki/User:Jmandel/blog
! http://www.openwfm.org/wiki/WRF-Fire_development_notes
! 


ADDITIONAL DOCUMENTATION IN THE doc SUBDIRECTORY


README.txt                  this file
README_git.txt              how to use the versioning system
README_mac.txt              how to run on a Mac
README_matlab_netcdf.txt    how to read WRF input and output directly in Matlab
README_vis.txt              matlab visualization using files written every timestep
README_visualization.txt    convert WRF input and output to Matlab
README_wps.txt              how use real data including fuel from Landfire

