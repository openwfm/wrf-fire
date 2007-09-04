module module_fr_spread_model
!
!*** Jan Mandel September 2007 email: jmandel@ucar.edu or Jan.Mandel@gmail.com
!
contains

subroutine fire_spread_model &
                (ids,ide, kds,kde, jds,jde,                    & ! incoming
                 ims,ime, kms,kme, jms,jme,                    &
                 its,ite, kts,kte, jts,jte,                    &
                 ifds,ifde, kfds,kfde, jfds,jfde,              &
                 ifms,ifme, kfms,kfme, jfms,jfme,              &
                 itimestep,dt,dx,dy,                           &
                 grid_id,cen_lat,cen_lon,lat_ll,lon_ll,        &
                 moad_cen_lat,moad_cen_lon,                    &
                 moad_lat_ll,moad_lon_ll,moad_dx,moad_dy,      &
                 moad_s_we,moad_e_we,moad_s_sn,moad_e_sn,      &
                 nfrx,nfry,                                    &
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


use module_fr_spread_core
use module_fr_spread_util
implicit none



end subroutine fire_spread_model

end module module_fr_spread_model
