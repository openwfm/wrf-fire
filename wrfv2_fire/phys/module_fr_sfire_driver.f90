/* Copyright (C) 1991-2012 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* We do support the IEC 559 math functionality, real and complex.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
/* Copyright (C) 1991-2012 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */
/* This header is separate from features.h so that the compiler can
   include it implicitly at the start of every compilation.  It must
   not itself include <features.h> or any other header that includes
   <features.h> because the implicit include comes before any feature
   test macros that may be defined in a source file before it first
   explicitly includes a system header.  GCC knows the name of this
   header in order to preinclude it.  */
/* We do support the IEC 559 math functionality, real and complex.  */
/* wchar_t uses ISO/IEC 10646 (2nd ed., published 2011-03-15) /
   Unicode 6.0.  */
/* We do not support C11 <threads.h>.  */
module module_fr_sfire_driver
use module_fr_sfire_model, only: sfire_model
use module_fr_sfire_phys, only: fire_params, init_fuel_cats, fuel_moisture, &
   advance_moisture, moisture_classes, &
   fire_rate_of_spread
use module_fr_sfire_atm, only: apply_windrf,interpolate_wind2fire_height,interpolate_atm2fire, &
   interpolate_z2fire,setup_wind_log_interpolation,interpolate_wind2can_height,massman_fwh
use module_fr_sfire_util
    USE module_dm , ONLY : ntasks_x,ntasks_y,local_communicator,mytask,ntasks,wrf_dm_sum_reals
    USE module_comm_dm , ONLY : halo_fire_fuel_sub, halo_fire_tign_sub, halo_fire_wind_f_sub, &
halo_fire_wind_a_sub, halo_fire_ph_sub, halo_fire_zsf_sub, halo_fire_longlat_sub, &
halo_fire_phb_sub, halo_fire_z0_sub, halo_fire_lfn_sub, HALO_FIRE_LFN_OUT_sub, &
HALO_FIRE_MAG_sub, HALO_FIRE_MFG_sub, halo_fire_ndwi_sub
use module_fr_sfire_atm, only: read_emissions_table, add_fire_emissions
USE module_domain, only: domain
USE module_configure, only: grid_config_rec_type
use module_model_constants, only: reradius, &
                                  pi2
implicit none
private
public sfire_driver_em,use_atm_vars,set_flags, &
       set_fp_from_grid, fire_ignition_convert
public ifun_beg, ifun_step, ifun_end
logical:: use_atm_vars=.true.
logical:: interpolate_long_lat=.true.
logical:: fmoist_run, fmoist_interp, fire_run
integer, parameter:: ifun_beg=1, ifun_step=3, ifun_end=6
contains
subroutine sfire_driver_em ( grid , config_flags &
            ,time_step_start,dt &
            ,fire_ifun_start,fire_ifun_end,tsteps &
            ,ids,ide, kds,kde, jds,jde &
            ,ims,ime, kms,kme, jms,jme &
            ,ips,ipe, kps,kpe, jps,jpe &
            ,ifds,ifde, jfds,jfde &
            ,ifms,ifme, jfms,jfme &
            ,ifps,ifpe, jfps,jfpe &
            ,rho,z_at_w,dz8w &
)
    implicit none
    TYPE(domain) , TARGET :: grid
    TYPE (grid_config_rec_type) , INTENT(IN) :: config_flags
    real, intent(in):: time_step_start, dt
    integer, intent(in):: fire_ifun_start,fire_ifun_end,tsteps
    integer, intent(in):: &
             ids,ide, kds,kde, jds,jde, &
             ims,ime, kms,kme, jms,jme, &
             ips,ipe, kps,kpe, jps,jpe, &
             ifds,ifde, jfds,jfde, &
             ifms,ifme, jfms,jfme, &
             ifps,ifpe, jfps,jfpe
    real,dimension(ims:ime, kms:kme, jms:jme),intent(in), optional::rho,z_at_w,dz8w
    TYPE(lines_type):: ignition, hfx
    integer::fire_ifun,ir,jr,istep,itimestep,i,ipe1,kpe1,jpe1,j
    logical::restart,replay
    real:: corner_ll,corner_ul,corner_ur,corner_lr, max_u, max_v, max_w, max_rho, min_rho
    character(len=128)msg
    type(fire_params)::fp
    real:: moisture_time
    logical:: run_advance_moisture,run_fuel_moisture, moisture_initializing
    real:: dt_moisture
    call sfire_debug_hook(config_flags%fire_debug_hook_sec)
    call time_start
    if(fire_ifun_start.le.1)call print_id
    call set_fp_from_grid(grid,fp)
    call set_flags(config_flags)
    call fire_ignition_convert (config_flags,ignition, &
             grid%fxlong, grid%fxlat, &
             ifds,ifde, jfds,jfde, &
             ifms,ifme, jfms,jfme, &
             ifps,ifpe, jfps,jfpe )
    call fire_hfx_convert (config_flags,hfx)
    grid%unit_fxlong = ignition%unit_fxlong
    grid%unit_fxlat = ignition%unit_fxlat
    if(fire_print_msg.ge.2 .and. fire_ifun_start .gt. 1)then
      ipe1=min(ide-1,ipe)
      jpe1=min(jde-1,jpe)
      kpe1=kpe-1
      max_u=fun_real(REAL_AMAX, &
        ims,ime,kms,kme,jms,jme, &
        ids,ide,kds,kde,jds,jde, &
        ips,ipe1,kps,kpe1,jps,jpe1, &
        1,0,0, &
        grid%u_2,grid%u_2)
      max_v=fun_real(REAL_AMAX, &
        ims,ime,kms,kme,jms,jme, &
        ids,ide,kds,kde,jds,jde, &
        ips,ipe1,kps,kpe1,jps,jpe1, &
        0,0,1, &
        grid%v_2,grid%v_2)
      max_w=fun_real(REAL_AMAX, &
        ims,ime,kms,kme,jms,jme, &
        ids,ide,kds,kde,jds,jde, &
        ips,ipe1,kps,kpe1,jps,jpe1, &
        0,1,0, &
        grid%w_2,grid%w_2)
      write(msg,91)time_step_start,'Maximal u wind      ',max_u,'m/s'
      call message(msg,0)
      write(msg,91)time_step_start,'Maximal v wind      ',max_v,'m/s'
      call message(msg,0)
      write(msg,91)time_step_start,'Maximal w wind      ',max_w,'m/s'
      call message(msg,0)
      if (present(rho)) then
        max_rho=fun_real(REAL_MAX, &
          ims,ime,kms,kme,jms,jme, &
          ids,ide,kds,kde,jds,jde, &
          ips,ipe1,kps,kpe1,jps,jpe1, &
          0,0,0, &
          rho,rho)
        min_rho=fun_real(REAL_MIN, &
          ims,ime,kms,kme,jms,jme, &
          ids,ide,kds,kde,jds,jde, &
          ips,ipe1,kps,kpe1,jps,jpe1, &
          0,0,0, &
          rho,rho)
        write(msg,91)time_step_start,'Minimal rho         ',min_rho,'kg/m^3'
        call message(msg,0)
        write(msg,91)time_step_start,'Maximal rho         ',max_rho,'kg/m^3'
        call message(msg,0)
      endif
93 format('Time ',f11.3,' s ',a,3e12.3,1x,a)
92 format('Time ',f11.3,' s ',a,2e12.3,1x,a)
91 format('Time ',f11.3,' s ',a,e12.3,1x,a)
    endif
    ir=grid%sr_x
    jr=grid%sr_y
    write(msg,'(a,2i4)')'fire mesh refinement ratios', ir,jr
    call message(msg)
    if(ir.le.0.or.jr.le.0)then
        call crash('fire mesh refinement ratio must be positive')
    endif
    itimestep=grid%itimestep
    restart=config_flags%restart .or. config_flags%cycling .or. config_flags%fire_restart
    replay= time_step_start+dt .le. config_flags%fire_perimeter_time
    fmoist_run = config_flags%fmoist_run
    fmoist_interp = config_flags%fmoist_interp
    if(fire_fmc_read.ne.0.and.fmoist_run)call crash('fmoist_run=T requires fire_fmc_read=0')
    fire_run = .not. config_flags%fmoist_only
    moisture_time = time_step_start
    run_advance_moisture = .false.
    run_fuel_moisture = .false.
    moisture_initializing = fire_ifun_start < 3
    if(fmoist_run)then
        if(moisture_initializing)then
            if(fire_ifun_end>2)call crash('initialization must be run separately')
            grid%fmoist_lasttime=moisture_time
            grid%fmoist_nexttime=moisture_time
            call message('moisture initialization')
            run_advance_moisture = .true.
        else
            if(config_flags%fmoist_freq > 0)then
                if(mod(grid%itimestep,config_flags%fmoist_freq) .eq. 0)then
                    write(msg,'(a,i10,a,i10)')'moisture model runs because timestep ',grid%itimestep,' is a multiple of ',config_flags%fmoist_freq
                    call message(msg)
                    run_advance_moisture = .true.
                endif
            else
                if(.not. moisture_time < grid%fmoist_nexttime) then
                    write(msg,'(a,f12.2,a)')'moisture model runs because time ',grid%fmoist_nexttime,'s has arrived'
                    call message(msg)
                    run_advance_moisture = .true.
                endif
            endif
            if(run_advance_moisture)then
                dt_moisture = moisture_time - grid%fmoist_lasttime
                grid%fmoist_lasttime = moisture_time
                if(config_flags%fmoist_freq > 0)then
                    write(msg,'(a,f12.2,a,i10,a)')'moisture time step is ',dt_moisture,'s running every ',config_flags%fmoist_freq,' steps'
                    call message(msg)
                else
                    grid%fmoist_nexttime = moisture_time + config_flags%fmoist_dt
                    write(msg,'(a,f12.2,a,f12.2,a)')'moisture time step is ',dt_moisture,'s next run at ',grid%fmoist_nexttime,'s'
                    call message(msg)
                endif
                if(fmoist_interp)then
                    call message('moisture interpolation to fuels will run because moisture model does')
                    run_fuel_moisture=.true.
                endif
            endif
        endif
    elseif(itimestep.eq.1.and.fmoist_interp)then
            call message('initializing, moisture interpolation to fuels will run from input data')
            run_fuel_moisture=.true.
    endif
!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
    write(msg,'(a,i1,a,i1,a,l1,a,l1)') &
       'sfire_driver_em: ifun from ',fire_ifun_start,' to ',fire_ifun_end, &
           ' restart=',restart,' replay=',replay
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
    call message(msg)
    do istep=0,tsteps
      itimestep = grid%itimestep + istep
      do fire_ifun=fire_ifun_start,fire_ifun_end
       if(fire_run)then
        if(fire_ifun.eq.1)then
CALL HALO_FIRE_LONGLAT_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
CALL HALO_FIRE_PHB_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
CALL HALO_FIRE_Z0_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
        if(kfmc_ndwi > 0 .and. fndwi_from_ndwi .eq.1)then
CALL HALO_FIRE_NDWI_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
        endif
        elseif(fire_ifun.eq.2)then
CALL HALO_FIRE_ZSF_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
            if(replay)then
                call message('replay, halo exchange on lfn and tign')
CALL HALO_FIRE_LFN_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
CALL HALO_FIRE_TIGN_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
            endif
        elseif(fire_ifun.eq.3)then
CALL HALO_FIRE_WIND_A_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
CALL HALO_FIRE_PH_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
        elseif(fire_ifun.eq.4)then
CALL HALO_FIRE_WIND_F_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
            if(run_fuel_moisture)then
CALL HALO_FIRE_MFG_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
            endif
        elseif(fire_ifun.eq.5)then
CALL HALO_FIRE_LFN_OUT_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
        elseif(fire_ifun.eq.6)then
            call message('halo exchange on lfn width 2 and tign')
CALL HALO_FIRE_TIGN_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
CALL HALO_FIRE_LFN_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
        endif
       endif
        if(fire_ifun.eq.2)then
            write(msg,'(a,i4,a,i4)') 'chem_opt=', config_flags%chem_opt, &
               ' tracer_opt=',config_flags%tracer_opt
            call message(msg)
            if(config_flags%chem_opt>0 .or. config_flags%tracer_opt > 0)then
                call read_emissions_table(config_flags%chem_opt,config_flags%tracer_opt)
            endif
        endif
        call sfire_driver_phys ( &
            fire_ifun, &
            ids,ide-1, kds,kde, jds,jde-1, &
            ims,ime, kms,kme, jms,jme, &
            ips,min(ipe,ide-1), kps,kpe, jps,min(jpe,jde-1), &
            ifds,ifde-ir, jfds,jfde-jr, &
            ifms,ifme, jfms,jfme, &
            ifps,min(ifpe,ifde-ir), jfps,min(jfpe,jfde-jr), &
            ir,jr, &
            grid%num_tiles, &
            grid%i_start,min(grid%i_end,ide-1), &
            grid%j_start,min(grid%j_end,jde-1), &
            itimestep,restart,replay,config_flags%fire_fuel_read,config_flags%fire_fuel_cat, &
            time_step_start,dt,grid%dx,grid%dy, &
            grid%u_frame,grid%v_frame, &
            config_flags%fire_ext_grnd,config_flags%fire_ext_crwn,config_flags%fire_crwn_hgt, &
            ignition,hfx, &
            grid%u_2,grid%v_2, &
            grid%ph_2,grid%phb, &
            grid%z0, &
            grid%ht, &
            grid%xlong,grid%xlat, &
            grid%tign_in, &
            grid%lfn,grid%tign_g,grid%fuel_frac, &
            grid%fire_area, &
            grid%fuel_frac_burnt, &
            grid%lfn_out, &
            grid%avg_fuel_frac, &
            grid%grnhfx,grid%grnqfx,grid%canhfx,grid%canqfx, &
            grid%uah,grid%vah, &
            grid%fgrnhfx,grid%fgrnqfx,grid%fcanhfx,grid%fcanqfx, &
            grid%ros,grid%flineint,grid%flineint2, &
            grid%f_ros0,grid%f_rosx,grid%f_rosy,grid%f_ros,&
            grid%f_int,grid%f_lineint,grid%f_lineint2, &
            grid%f_ros11,grid%f_ros12,grid%f_ros13,grid%f_ros21, &
            grid%f_ros23,grid%f_ros31,grid%f_ros32,grid%f_ros33, &
            grid%fxlong,grid%fxlat, &
            grid%fire_hfx, &
            grid%nfuel_cat, &
            grid%fuel_time, &
            grid%wz0, &
            grid%fz0, grid%fwh,grid%can_top,grid%cuf,grid%cvf, &
            fp, &
            config_flags%nfmc, &
            run_advance_moisture,run_fuel_moisture,dt_moisture, &
            config_flags%fmep_decay_tlag, &
            grid%rainc, grid%rainnc, &
            grid%t2, grid%q2, grid%psfc, &
            grid%rain_old, &
            grid%t2_old, grid%q2_old, grid%psfc_old, &
            grid%rh_fire, &
            grid%fmc_gc, &
            grid%fmep, &
            grid%fmc_equi, &
            grid%fmc_lag, &
            fp%fmc_g, &
            grid%ndwi, &
            grid%fndwi)
          if(fire_run)then
              if(fire_ifun.eq.2)then
CALL HALO_FIRE_FUEL_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
                  call message('halo exchange on lfn width 2')
CALL HALO_FIRE_LFN_sub ( grid, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
              endif
              if(run_fuel_moisture)then
                  if(fire_ifun.eq.3)then
CALL HALO_FIRE_MAG_sub ( grid, &
  config_flags, &
  local_communicator, &
  mytask, ntasks, ntasks_x, ntasks_y, &
  ids, ide, jds, jde, kds, kde, &
  ims, ime, jms, jme, kms, kme, &
  ips, ipe, jps, jpe, kps, kpe )
                  endif
              endif
          endif
     if(fire_ifun.eq.6)then
         if(config_flags%chem_opt>0 .or. config_flags%tracer_opt>0)then
             if(.not.(present(rho).and.present(dz8w)))then
                 call crash('sfire_driver_em: must have rho and dz8w to call add_fire_emissions')
             endif
             call add_fire_emissions( &
                 config_flags%chem_opt,config_flags%tracer_opt,dt,grid%dx,grid%dy, &
                 ifms,ifme,jfms,jfme, &
                 ifps,ifpe,jfps,jfpe, &
                 ids,ide,kds,kde,jds,jde, &
                 ims,ime,kms,kme,jms,jme, &
                 ips,ipe,kps,kpe,jps,jpe, &
                 rho,dz8w, &
                 grid%fgip, grid%fuel_frac_burnt, grid%nfuel_cat, &
                 grid%chem,grid%tracer)
          endif
     endif
      enddo
    enddo
    if(tsteps>0)call crash('sfire_driver_em: test run of uncoupled fire model completed')
    call time_end('sfire')
end subroutine sfire_driver_em
subroutine sfire_driver_phys (ifun, &
    ids,ide, kds,kde, jds,jde, &
    ims,ime, kms,kme, jms,jme, &
    ips,ipe, kps,kpe, jps,jpe, &
    ifds, ifde, jfds, jfde, &
    ifms, ifme, jfms, jfme, &
    ifps, ifpe, jfps, jfpe, &
    ir,jr, &
    num_tiles,i_start,i_end,j_start,j_end, &
    itimestep,restart,replay,ifuelread,nfuel_cat0, &
    time_step_start,dt,dx,dy, &
    u_frame,v_frame, &
    fire_ext_grnd,fire_ext_crwn,fire_crwn_hgt, &
    ignition,hfx, &
    u,v, &
    ph,phb, &
    z0,zs, &
    xlong,xlat, &
    tign_in, &
    lfn,tign,fuel_frac, &
    fire_area, &
    fuel_frac_burnt, &
    lfn_out, &
    avg_fuel_frac, &
    grnhfx,grnqfx,canhfx,canqfx, &
    uah,vah, &
    fgrnhfx,fgrnqfx,fcanhfx,fcanqfx, &
    ros,flineint,flineint2, &
    f_ros0,f_rosx,f_rosy,f_ros, &
    f_int,f_lineint,f_lineint2, &
    f_ros11,f_ros12,f_ros13,f_ros21, &
    f_ros23,f_ros31,f_ros32,f_ros33, &
    fxlong,fxlat, &
    fire_hfx, &
    nfuel_cat, &
    fuel_time, &
    wz0, &
    fz0,fwh,can_top,cuf,cvf, &
    fp, &
    nfmc, &
    run_advance_moisture,run_fuel_moisture,dt_moisture,&
    fmep_decay_tlag, &
    rainc,rainnc, &
    t2, q2, psfc, &
    rain_old, &
    t2_old, q2_old, psfc_old, &
    rh_fire, &
    fmc_gc, &
    fmep, &
    fmc_equi, &
    fmc_lag, &
    fmc_g, &
    ndwi, &
    fndwi)
implicit none
integer, intent(in)::ifun, &
    ids,ide, kds,kde, jds,jde, &
    ims,ime, kms,kme, jms,jme, &
    ips,ipe, kps,kpe, jps,jpe, &
    ifds, ifde, jfds, jfde, &
    ifms, ifme, jfms, jfme, &
    ifps, ifpe, jfps, jfpe, &
    ir,jr, &
    nfmc, &
    itimestep, &
    ifuelread, &
    nfuel_cat0, &
    num_tiles
logical, intent(in)::restart,replay
integer,dimension(num_tiles),intent(in) :: i_start,i_end,j_start,j_end
real, intent(in):: &
    time_step_start, &
    dt, &
    dx,dy, &
    u_frame,v_frame, &
    fire_crwn_hgt, &
    fire_ext_grnd, &
    fire_ext_crwn
TYPE (lines_type), intent(inout):: ignition,hfx
real,intent(in),dimension(ims:ime,kms:kme,jms:jme)::u,v, &
                              ph, phb
real,intent(in),dimension(ims:ime, jms:jme):: z0, &
                                                zs
real,intent(out),dimension(ims:ime,jms:jme)::&
    uah, &
    vah
real, dimension(ims:ime, jms:jme), intent(inout)::xlong, xlat, ndwi
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: &
    fz0,fwh,can_top,cuf,cvf,wz0, &
    nfuel_cat,fndwi
real, intent(inout), dimension(ifms:ifme, jfms:jfme):: &
    tign_in, &
    lfn,tign,fuel_frac, &
    lfn_out
real, intent(out), dimension(ifms:ifme, jfms:jfme):: &
    fire_area, &
    fuel_frac_burnt
real, intent(out), dimension(ims:ime, jms:jme):: &
    avg_fuel_frac, &
    grnhfx, &
    grnqfx, &
    canhfx, &
    canqfx
real, intent(out), dimension(ifms:ifme, jfms:jfme):: &
    fgrnhfx, &
    fgrnqfx, &
    fcanhfx, &
    fcanqfx, &
    ros,flineint,flineint2, &
    f_ros0,f_rosx,f_rosy,f_ros, &
    f_int,f_lineint,f_lineint2, &
    f_ros11,f_ros12,f_ros13,f_ros21, &
    f_ros23,f_ros31,f_ros32,f_ros33
logical, intent(in)::run_advance_moisture,run_fuel_moisture
real, intent(in)::dt_moisture
real, intent(in)::fmep_decay_tlag
real, intent(in), dimension(ims:ime,jms:jme):: t2, q2, psfc, rainc, rainnc
real, intent(inout), dimension(ims:ime,jms:jme):: t2_old, q2_old, psfc_old, rain_old
real, intent(out),dimension(ims:ime,jms:jme):: rh_fire
real, intent(inout), dimension(ims:ime,nfmc,jms:jme):: fmc_gc
real, intent(inout), dimension(ims:ime,2,jms:jme):: fmep
real, intent(out), dimension(ims:ime,nfmc,jms:jme):: fmc_equi,fmc_lag
real, intent(inout), dimension(ifms:ifme,jfms:jfme):: fmc_g
real, dimension(ifms:ifme, jfms:jfme), intent(inout)::fxlong,fxlat, &
    fire_hfx
real, intent(out), dimension(ifms:ifme, jfms:jfme)::fuel_time
type(fire_params),intent(inout)::fp
real :: dxf,dyf,time_start,latm, s
integer :: its,ite,jts,jte,kts,kte, &
    ij,i,j,k,id,pid,ipe1,jpe1,ite1,jte1, &
    ii,jj, &
    ifts,ifte,jfts,jfte
character(len=128)::msg
character(len=3)::kk
real, parameter::zero=0.
time_start = time_step_start
dxf=dx/ir
dyf=dy/jr
!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
write(msg,'(a,i5)')'sfire_driver_phys stage ',ifun
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
write(msg,7001)'fire domain     ','ifds',ifds,ifde,jfds,jfde
call message(msg)
write(msg,7001)'fire memory     ','ifms',ifms,ifme,jfms,jfme
call message(msg)
write(msg,7001)'fire patch      ','ifps',ifps,ifpe,jfps,jfpe
call message(msg)
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
call check_fmesh(ids,ide,ifds,ifde,ir,'id')
call check_fmesh(jds,jde,jfds,jfde,jr,'jd')
call check_fmesh(ips,ipe,ifps,ifpe,ir,'ip')
call check_fmesh(jps,jpe,jfps,jfpe,jr,'jp')
call check_mesh_2dim(ips,ipe,jps,jpe,ims,ime,jms,jme)
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme)
call check_mesh_2dim(ips,ipe,jps,jpe,ids,ide,jds,jde)
call check_mesh_2dim(ifps,ifpe,jfps,jfpe,ifds,ifde,jfds,jfde)
pid=0
if(fire_print_file.gt.0)then
    if(itimestep.le.fire_print_file.or.mod(itimestep,fire_print_file).eq.0)pid=itimestep
endif
if(ifun.eq.1)then
         call init_fuel_cats(fmoist_run .or. fmoist_interp)
endif
if(ifun.eq.3)then
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,1,0,0,u,'u')
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,0,0,1,v,'v')
 call print_chsum(itimestep,ims,ime,kms,kme,jms,jme,ids,ide,kds,kde,jds,jde,ips,ipe,kps,kpe,jps,jpe,0,1,0,ph,'ph')
endif
call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,lfn,'lfn')
call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,tign,'tign')
kts=kps
kte=kpe
ipe1=ifval(ipe.eq.ide,ipe+1,ipe)
jpe1=ifval(jpe.eq.jde,jpe+1,jpe)
!$OMP PARALLEL DO PRIVATE(ij,its,ite,jts,jte,ite1,jte1,ifts,ifte,jfts,jfte,msg,id) &
!$OMP SCHEDULE(STATIC)
do ij=1,num_tiles
    id = ifval(pid.ne.0,pid+(ij-1)*10000,0)
    its = i_start(ij)
    ite = i_end(ij)
    jts = j_start(ij)
    jte = j_end(ij)
    ifts= (its-ids)*ir+ifds
    ifte= (ite-ids+1)*ir+ifds-1
    jfts= (jts-jds)*jr+jfds
    jfte= (jte-jds+1)*jr+jfds-1
    ite1=ifval(ite.eq.ide,ite+1,ite)
    jte1=ifval(jte.eq.jde,jte+1,jte)
!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
    write(msg,'(a,i3,1x,a,i7,1x,a,i3)')'tile=',ij,' id=',id,' ifun=',ifun
    call message(msg)
    write(msg,7001)'atm tile   ','its',its,ite,jts,jte
    call message(msg)
    write(msg,7001)'fire tile  ','ifts',ifts,ifte,jfts,jfte
    call message(msg)
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
    call check_mesh_2dim(its,ite,jts,jte,ips,ipe,jps,jpe)
    call check_mesh_2dim(ifts,ifte,jfts,jfte,ifps,ifpe,jfps,jfpe)
    call check_mesh_2dim(ifts-2,ifte+2,jfts-2,jfte+2,ifms,ifme,jfms,jfme)
!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
    write(msg,'(a,i6,a,2(f15.6,a))')'time step',itimestep,' at',time_start,' duration',dt,'s'
    call message(msg)
    7001 format(a,' dimensions ',a4,':',i6,' to ',i6,' by ',i6,' to ',i6)
    write(msg,'(a,2i9)')'refinement ratio:',ir,jr
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
    if(run_advance_moisture)then
      if(ifun.eq.3)then
          call message('advance_moisture start')
          call advance_moisture( &
              itimestep.eq.1, &
              ims,ime, jms,jme, &
              its,ite, jts,jte, &
              nfmc, &
              dt_moisture, &
              fmep_decay_tlag, &
              rainc, rainnc, &
              t2, q2, psfc, &
              rain_old, &
              t2_old, q2_old, psfc_old, &
              rh_fire, &
              fmc_gc, &
              fmep, &
              fmc_equi, &
              fmc_lag &
          )
          call message('advance_moisture end')
      endif
    endif
    if(fire_run)then
     if(ifun.eq.2)then
      if(restart)then
          call message('restart - interpolation skipped')
      else
        if(kfmc_ndwi > 0 .and. fndwi_from_ndwi .eq.1)then
            call print_2d_stats(ips,ipe,jps,jpe,ims,ime,jms,jme,fndwi,'driver:ndwi')
            call interpolate_z2fire(id,0, &
                ids,ide, jds,jde, &
                ims,ime, jms,jme, &
                ips,ipe,jps,jpe, &
                its,ite,jts,jte, &
                ifds, ifde, jfds, jfde, &
                ifms, ifme, jfms, jfme, &
                ifts,ifte,jfts,jfte, &
                ir,jr, &
                ndwi, &
                fndwi)
            call print_2d_stats(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fndwi,'driver:fndwi')
         endif
!!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
!!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
        if(ignition%longlat .eq.0)then
        elseif(use_atm_vars)then
         call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,xlat,'xlat',id)
         call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,xlong,'xlong',id)
        if (interpolate_long_lat)then
         call message('Intepolating node longitude and latitude to fire mesh')
         call interpolate_z2fire(id,1, &
            ids,ide, jds,jde, &
            ims,ime, jms,jme, &
            ips,ipe,jps,jpe, &
            its,ite,jts,jte, &
            ifds, ifde, jfds, jfde, &
            ifms, ifme, jfms, jfme, &
            ifts,ifte,jfts,jfte, &
            ir,jr, &
            xlat, &
            fxlat)
        call interpolate_z2fire(id,1, &
            ids,ide, jds,jde, &
            ims,ime, jms,jme, &
            ips,ipe,jps,jpe, &
            its,ite,jts,jte, &
            ifds, ifde, jfds, jfde, &
            ifms, ifme, jfms, jfme, &
            ifts,ifte,jfts,jfte, &
            ir,jr, &
            xlong, &
            fxlong)
        endif
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fp%zsf,'driver_phys:zsf')
        endif
     endif
    elseif(ifun.eq.3)then
      if(use_atm_vars)then
        call write_array_m(its,ite,jts,jte,ims,ime,jms,jme,z0,'z0',id)
        call write_array_m3(its,ite1,kts,kde-1,jts,jte,ims,ime,kms,kme,jms,jme,u,'u_2',id)
        call write_array_m3(its,ite,kts,kde-1,jts,jte1,ims,ime,kms,kme,jms,jme,v,'v_2',id)
        call write_array_m3(its,ite,kts,kde,jts,jte,ims,ime,kms,kme,jms,jme,ph,'ph_2',id)
        call write_array_m3(its,ite,kts,kde,jts,jte,ims,ime,kms,kme,jms,jme,phb,'phb',id)
        if(fire_wind_log_interp.eq.4)then
          call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,z0,'driver_phys:z0')
          call interpolate_atm2fire(id, &
            ids,ide, kds,kde, jds,jde, &
            ims,ime, kms,kme, jms,jme, &
            ips,ipe, jps,jpe, &
            its,ite,jts,jte, &
            ifds, ifde, jfds, jfde, &
            ifms, ifme, jfms, jfme, &
            ifps, ifpe, jfps, jfpe, &
            ifts, ifte, jfts, jfte, &
            ir,jr, &
            u_frame, v_frame, &
            u,v, &
            ph,phb, &
            z0,zs, &
            uah,vah, &
            fp%vx,fp%vy)
          call apply_windrf( &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            nfuel_cat,fp%vx,fp%vy)
            wz0 = 0
        else
          call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fz0,'driver_phys:fz0')
          call interpolate_wind2fire_height(id, &
            ids,ide, kds,kde, jds,jde, &
            ims,ime, kms,kme, jms,jme, &
            ips,ipe,jps,jpe, &
            its,ite,jts,jte, &
            ifds, ifde, jfds, jfde, &
            ifms, ifme, jfms, jfme, &
            ifps, ifpe, jfps, jfpe, &
            ifts,ifte,jfts,jfte, &
            ir,jr, &
            u_frame, v_frame, &
            u,v,ph,phb, &
            fz0,fwh, &
            fp%vx,fp%vy)
          if(fire_use_windrf.eq.1)then
            call apply_windrf( &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            nfuel_cat,fp%vx,fp%vy)
          endif
          if(fire_use_windrf.eq.4)then
            write(*,*)'fire_use_windrf option 4 has been selected....!'
            write(*,*)'fire_can_top_read=',fire_can_top_read
            can_top = 22.5
            call interpolate_wind2can_height(id, &
            ids,ide, kds,kde, jds,jde, &
            ims,ime, kms,kme, jms,jme, &
            ips,ipe,jps,jpe, &
            its,ite,jts,jte, &
            ifds, ifde, jfds, jfde, &
            ifms, ifme, jfms, jfme, &
            ifps, ifpe, jfps, jfpe, &
            ifts,ifte,jfts,jfte, &
            ir,jr, &
            u_frame, v_frame, &
            u,v,ph,phb, &
            wz0,can_top, &
            cuf,cvf)
            call massman_fwh(id, &
            ids,ide, kds,kde, jds,jde, &
            ims,ime, kms,kme, jms,jme, &
            ips,ipe,jps,jpe, &
            its,ite,jts,jte, &
            ifds, ifde, jfds, jfde, &
            ifms, ifme, jfms, jfme, &
            ifps, ifpe, jfps, jfpe, &
            ifts,ifte,jfts,jfte, &
            ir,jr, &
            cuf,cvf, &
            nfuel_cat, &
            can_top,fwh, &
            fp%vx,fp%vy)
          endif
        endif
      endif
    elseif(ifun.eq.4)then
      if(run_fuel_moisture)then
        call message('fuel_moisture start')
        call fuel_moisture( &
        id, &
        nfmc, &
        ids,ide, jds,jde, &
        ims,ime, jms,jme, &
        ips,ipe,jps,jpe, &
        its,ite,jts,jte, &
        ifds, ifde, jfds, jfde, &
        ifms, ifme, jfms, jfme, &
        ifts,ifte,jfts,jfte, &
        ir,jr, &
        nfuel_cat, &
        fndwi, &
        fmc_gc, &
        fmc_g &
        )
        call message('fuel_moisture end')
      endif
      call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fmc_g,'driver_phys:fmc_g')
    endif
    call sfire_model (id,ifun,restart,replay, &
        run_fuel_moisture, &
        ifuelread,nfuel_cat0, &
        ifds,ifde,jfds,jfde, &
        ifms,ifme,jfms,jfme, &
        ifps,ifpe,jfps,jfpe, &
        ifts,ifte,jfts,jfte, &
        time_start,dt, &
        dxf,dyf, &
        ignition,hfx, &
        fxlong,fxlat, &
        fire_hfx, &
        tign_in, &
        lfn,lfn_out,tign,fuel_frac, &
        fire_area, &
        fuel_frac_burnt, &
        fgrnhfx,fgrnqfx, &
        ros,flineint,flineint2, &
        f_ros0,f_rosx,f_rosy,f_ros, &
        f_int,f_lineint,f_lineint2, &
        nfuel_cat, &
        fuel_time,fwh,fz0, &
        fp &
    )
     if(ifun.eq.2)then
        call setup_wind_log_interpolation( &
            ids,ide, jds,jde, &
            ims,ime, jms,jme, &
            ips,ipe,jps,jpe, &
            its,ite,jts,jte, &
            ifds, ifde, jfds, jfde, &
            ifms, ifme, jfms, jfme, &
            ifts,ifte,jfts,jfte, &
            ir,jr, &
            z0, &
            nfuel_cat, &
            fz0,fwh,wz0)
    elseif(ifun.eq.6)then
    do j=jfts,jfte
       do i=ifts,ifte
          f_ros11(i,j)=fire_rate_of_spread( dxf*(1-2), dyf*(1-2), i,j,fp)
          f_ros12(i,j)=fire_rate_of_spread( dxf*(1-2), dyf*(2-2), i,j,fp)
          f_ros13(i,j)=fire_rate_of_spread( dxf*(1-2), dyf*(3-2), i,j,fp)
          f_ros21(i,j)=fire_rate_of_spread( dxf*(2-2), dyf*(1-2), i,j,fp)
          f_ros23(i,j)=fire_rate_of_spread( dxf*(2-2), dyf*(3-2), i,j,fp)
          f_ros31(i,j)=fire_rate_of_spread( dxf*(3-2), dyf*(1-2), i,j,fp)
          f_ros32(i,j)=fire_rate_of_spread( dxf*(3-2), dyf*(2-2), i,j,fp)
          f_ros33(i,j)=fire_rate_of_spread( dxf*(3-2), dyf*(3-2), i,j,fp)
       enddo
    enddo
    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,f_ros11,'driver_phys:f_ros11')
    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,f_ros12,'driver_phys:f_ros12')
    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,f_ros13,'driver_phys:f_ros13')
    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,f_ros21,'driver_phys:f_ros21')
    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,f_ros23,'driver_phys:f_ros23')
    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,f_ros31,'driver_phys:f_ros31')
    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,f_ros32,'driver_phys:f_ros32')
    call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,f_ros33,'driver_phys:f_ros33')
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,ros,'sfire_driver:ros')
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnhfx,'sfire_driver:fgrnhfx')
        call print_2d_stats(ifts,ifte,jfts,jfte,ifms,ifme,jfms,jfme,fgrnqfx,'sfire_driver:fgrnqfx')
        if(use_atm_vars)then
          call sum_2d_cells( &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            fuel_frac, &
            ims, ime, jms, jme, &
            its,ite,jts,jte, &
            avg_fuel_frac)
          call sum_2d_cells( &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            fgrnhfx, &
            ims, ime, jms, jme, &
            its,ite,jts,jte, &
            grnhfx)
          call sum_2d_cells( &
            ifms,ifme,jfms,jfme, &
            ifts,ifte,jfts,jfte, &
            fgrnqfx, &
            ims, ime, jms, jme, &
            its,ite,jts,jte, &
            grnqfx)
!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
          write(msg,'(a,f6.3)')'fire-atmosphere feedback scaling ',fire_atm_feedback
!$OMP end CRITICAL(SFIRE_DRIVER_CRIT)
   call message(msg)
          s = 1./(ir*jr)
          do j=jts,jte
            do i=its,ite
                avg_fuel_frac(i,j)=avg_fuel_frac(i,j)*s
                grnhfx(i,j)=fire_atm_feedback*grnhfx(i,j)*s
                grnqfx(i,j)=fire_atm_feedback*grnqfx(i,j)*s
                canhfx(i,j)=0
                canqfx(i,j)=0
            enddo
          enddo
          call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnhfx,'fire_driver:grnhfx')
          call print_2d_stats(its,ite,jts,jte,ims,ime,jms,jme,grnqfx,'fire_driver:grnqfx')
       endif
    endif
  endif
enddo
!$OMP END PARALLEL DO
if(ifun.eq.1)then
    if(pid.ne.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,zs,'zs',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%zsf,'zsf',pid)
    endif
endif
if (ifun.eq.3)then
 call print_3d_stats_by_slice(ips,ipe,1,moisture_classes,jps,jpe,ims,ime,1,nfmc,jms,jme,fmc_gc,'fmc_gc')
 call print_chsum(itimestep,ims,ime,1,nfmc,jms,jme,ids,ide,1,moisture_classes,jds,jde,ips,ipe,1,moisture_classes,jps,jpe,0,0,0,fmc_gc,'fmc_gc')
endif
if (ifun.eq.4)then
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fmc_g,'fmc_g')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fp%vx,'uf')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fp%vy,'vf')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,lfn,'lfn')
 call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,tign,'tign')
    if(pid.gt.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnhfx,'grnhfx',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnqfx,'grnqfx',pid)
        call write_array_m3(ips,ipe1,kds,kde+1,jps,jpe,ims,ime,kms,kme,jms,jme,u,'u',pid)
        call write_array_m3(ips,ipe,kds,kde+1,jps,jpe1,ims,ime,kms,kme,jms,jme,v,'v',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%vx,'uf',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fp%vy,'vf',pid)
    endif
endif
if(ifun.eq.5)then
    if(pid.gt.0)then
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,lfn,'lfn',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,tign,'tign',pid)
    endif
endif
if(ifun.eq.6)then
    call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fgrnhfx,'fgrnhfx')
    call print_chsum(itimestep,ifms,ifme,1,1,jfms,jfme,ifds,ifde,1,1,jfds,jfde,ifps,ifpe,1,1,jfps,jfpe,0,0,0,fgrnqfx,'fgrnqfx')
    call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,0,grnhfx,'grnhfx')
    call print_chsum(itimestep,ims,ime,1,1,jms,jme,ids,ide,1,1,jds,jde,ips,ipe,1,1,jps,jpe,0,0,0,grnqfx,'grnqfx')
    if(pid.gt.0)then
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnhfx,'grnhfx',pid)
        call write_array_m(ips,ipe,jps,jpe,ims,ime,jms,jme,grnqfx,'grnqfx',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fuel_frac,'fuel_frac',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnhfx,'fgrnhfx',pid)
        call write_array_m(ifps,ifpe,jfps,jfpe,ifms,ifme,jfms,jfme,fgrnqfx,'fgrnqfx',pid)
    endif
endif
end subroutine sfire_driver_phys
subroutine check_fmesh(ids,ide,ifds,ifde,ir,s)
implicit none
integer, intent(in)::ids,ide,ifds,ifde,ir
character(len=*),intent(in)::s
character(len=128)msg
if ((ide-ids+1)*ir.ne.(ifde-ifds+1))then
!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
    write(msg,1)s,ids,ide,ifds,ifde,ir
1 format('module_fr_sfire_driver: incompatible bounds ',a,' atm ',i5,':',i5,' fire ',i5,':',i5,' ratio ',i3)
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
    call crash(msg)
endif
end subroutine check_fmesh
subroutine fire_ignition_convert (config_flags,ignition, &
             fxlong, fxlat, &
             ifds,ifde, jfds,jfde, &
             ifms,ifme, jfms,jfme, &
             ifps,ifpe, jfps,jfpe )
    implicit none
    TYPE (grid_config_rec_type) , INTENT(IN) :: config_flags
    TYPE (lines_type), INTENT(OUT):: ignition
    integer::ifds,ifde, jfds,jfde, &
             ifms,ifme, jfms,jfme, &
             ifps,ifpe, jfps,jfpe
    real, dimension(ifms:ifme,jfms:jfme):: fxlong,fxlat
    integer::i,j,ii,jj
    logical:: real,ideal
    character(len=128)msg
    real:: corner_longlat(2,2,2), corner_longlat_1(8), corner_longlat_2(8),lon(2),lat(2)
    real, dimension(2,2):: corner_long,corner_lat
    ignition%max_lines=5
    ignition%num_lines=config_flags%fire_num_ignitions
    if(fire_max_lines.lt.ignition%max_lines)call crash('fire_max_lines too small')
    ideal=config_flags%fire_ignition_start_x1 .ne.0. .or. config_flags%fire_ignition_start_y1 .ne. 0.
    real=config_flags%fire_ignition_start_lon1 .ne. 0. .or. config_flags%fire_ignition_start_lat1 .ne. 0.
    if(ideal)call message('Using ideal ignition coordinates, m from the lower left domain corner')
    if(real)call message('Using real ignition coordinates, longitude and latitude')
    if(ideal.and.real)call crash('Only one of the ideal or real coordinates may be given')
    ignition%longlat=0
    if(ideal)then
       ignition%longlat=0
       ignition%line(1)%start_x=config_flags%fire_ignition_start_x1
       ignition%line(1)%start_y=config_flags%fire_ignition_start_y1
       ignition%line(1)%end_x=config_flags%fire_ignition_end_x1
       ignition%line(1)%end_y=config_flags%fire_ignition_end_y1
       ignition%line(2)%start_x=config_flags%fire_ignition_start_x2
       ignition%line(2)%start_y=config_flags%fire_ignition_start_y2
       ignition%line(2)%end_x=config_flags%fire_ignition_end_x2
       ignition%line(2)%end_y=config_flags%fire_ignition_end_y2
       ignition%line(3)%start_x=config_flags%fire_ignition_start_x3
       ignition%line(3)%start_y=config_flags%fire_ignition_start_y3
       ignition%line(3)%end_x=config_flags%fire_ignition_end_x3
       ignition%line(3)%end_y=config_flags%fire_ignition_end_y3
       ignition%line(4)%start_x=config_flags%fire_ignition_start_x4
       ignition%line(4)%start_y=config_flags%fire_ignition_start_y4
       ignition%line(4)%end_x=config_flags%fire_ignition_end_x4
       ignition%line(4)%end_y=config_flags%fire_ignition_end_y4
       ignition%line(5)%start_x=config_flags%fire_ignition_start_x5
       ignition%line(5)%start_y=config_flags%fire_ignition_start_y5
       ignition%line(5)%end_x=config_flags%fire_ignition_end_x5
       ignition%line(5)%end_y=config_flags%fire_ignition_end_y5
    endif
    if(real)then
       ignition%longlat=1
       ignition%line(1)%start_x=config_flags%fire_ignition_start_lon1
       ignition%line(1)%start_y=config_flags%fire_ignition_start_lat1
       ignition%line(1)%end_x=config_flags%fire_ignition_end_lon1
       ignition%line(1)%end_y=config_flags%fire_ignition_end_lat1
       ignition%line(2)%start_x=config_flags%fire_ignition_start_lon2
       ignition%line(2)%start_y=config_flags%fire_ignition_start_lat2
       ignition%line(2)%end_x=config_flags%fire_ignition_end_lon2
       ignition%line(2)%end_y=config_flags%fire_ignition_end_lat2
       ignition%line(3)%start_x=config_flags%fire_ignition_start_lon3
       ignition%line(3)%start_y=config_flags%fire_ignition_start_lat3
       ignition%line(3)%end_x=config_flags%fire_ignition_end_lon3
       ignition%line(3)%end_y=config_flags%fire_ignition_end_lat3
       ignition%line(4)%start_x=config_flags%fire_ignition_start_lon4
       ignition%line(4)%start_y=config_flags%fire_ignition_start_lat4
       ignition%line(4)%end_x=config_flags%fire_ignition_end_lon4
       ignition%line(4)%end_y=config_flags%fire_ignition_end_lat4
       ignition%line(5)%start_x=config_flags%fire_ignition_start_lon5
       ignition%line(5)%start_y=config_flags%fire_ignition_start_lat5
       ignition%line(5)%end_x=config_flags%fire_ignition_end_lon5
       ignition%line(5)%end_y=config_flags%fire_ignition_end_lat5
    endif
       ignition%line(1)%ros=config_flags%fire_ignition_ros1
       ignition%line(1)%radius=config_flags%fire_ignition_radius1
       ignition%line(1)%start_time=config_flags%fire_ignition_start_time1
       ignition%line(1)%end_time=config_flags%fire_ignition_end_time1
       ignition%line(2)%ros=config_flags%fire_ignition_ros2
       ignition%line(2)%radius=config_flags%fire_ignition_radius2
       ignition%line(2)%start_time=config_flags%fire_ignition_start_time2
       ignition%line(2)%end_time=config_flags%fire_ignition_end_time2
       ignition%line(3)%ros=config_flags%fire_ignition_ros3
       ignition%line(3)%radius=config_flags%fire_ignition_radius3
       ignition%line(3)%start_time=config_flags%fire_ignition_start_time3
       ignition%line(3)%end_time=config_flags%fire_ignition_end_time3
       ignition%line(4)%ros=config_flags%fire_ignition_ros4
       ignition%line(4)%radius=config_flags%fire_ignition_radius4
       ignition%line(4)%start_time=config_flags%fire_ignition_start_time4
       ignition%line(4)%end_time=config_flags%fire_ignition_end_time4
       ignition%line(5)%ros=config_flags%fire_ignition_ros5
       ignition%line(5)%radius=config_flags%fire_ignition_radius5
       ignition%line(5)%start_time=config_flags%fire_ignition_start_time5
       ignition%line(5)%end_time=config_flags%fire_ignition_end_time5
       call postprocess_lines(ignition,'ros',config_flags)
        corner_longlat=0.
        if(ifds.eq.ifps.and.jfds.eq.jfps)then
            corner_longlat(1,1,1)=fxlong(ifps,jfps)
            corner_longlat(1,1,2)=fxlat(ifps,jfps)
        endif
        if(ifds.eq.ifps.and.jfde.eq.jfpe)then
            corner_longlat(1,2,1)=fxlong(ifps,jfpe)
            corner_longlat(1,2,2)=fxlat(ifps,jfpe)
        endif
        if(ifde.eq.ifpe.and.jfds.eq.jfps)then
            corner_longlat(2,1,1)=fxlong(ifpe,jfps)
            corner_longlat(2,1,2)=fxlat(ifpe,jfps)
        endif
        if(ifde.eq.ifpe.and.jfde.eq.jfpe)then
            corner_longlat(2,2,1)=fxlong(ifpe,jfpe)
            corner_longlat(2,2,2)=fxlat(ifpe,jfpe)
        endif
        corner_longlat_1=reshape(corner_longlat,(/8/))
        call wrf_dm_sum_reals(corner_longlat_1,corner_longlat_2)
        corner_longlat=reshape(corner_longlat_2,(/2,2,2/))
        corner_long=corner_longlat(1:2,1:2,1)
        corner_lat=corner_longlat(1:2,1:2,2)
        if(fire_print_msg.ge.2)then
            do i=1,2
                do j=1,2
                     write(msg,'(a,2i2,a,2f14.8)')'corner',i,j,' coordinates ',corner_long(i,j),corner_lat(i,j)
                     call message(msg)
                enddo
            enddo
        endif
 lon(1)=(corner_long(1,1)+corner_long(1,2))/2.
 lon(2)=(corner_long(2,1)+corner_long(2,2))/2.
 lat(1)=(corner_lat(1,1)+corner_lat(2,1))/2.
 lat(2)=(corner_lat(1,2)+corner_lat(2,2))/2.
        if(fire_print_msg.ge.2)then
            write(msg,'(4(a,f14.8))')'coordinates ',lon(1),':',lon(2),',',lat(1),':',lat(2)
            call message(msg)
        endif
       do i=1,ignition%num_lines
           call check_ignition_coordinate(ignition%line(i)%start_x,lon(1),lon(2))
           call check_ignition_coordinate(ignition%line(i)%start_y,lat(1),lat(2))
           call check_ignition_coordinate(ignition%line(i)%end_x,lon(1),lon(2))
           call check_ignition_coordinate(ignition%line(i)%end_y,lat(1),lat(2))
       enddo
       if (fire_ignition_clamp>0) then
       do i=1,ignition%num_lines
           call clamp_to_grid(ignition%line(i)%start_x,lon(1),lon(2),ifds,ifde,ignition%line(i)%start_x,ii)
           call clamp_to_grid(ignition%line(i)%start_y,lat(1),lat(2),jfds,jfde,ignition%line(i)%start_y,jj)
           call display_clamp
           call clamp_to_grid(ignition%line(i)%end_x,lon(1),lon(2),ifds,ifde,ignition%line(i)%end_x,ii)
           call clamp_to_grid(ignition%line(i)%end_y,lat(1),lat(2),jfds,jfde,ignition%line(i)%end_y,jj)
           call display_clamp
       enddo
       endif
       contains
       subroutine display_clamp
        character(len=128)::msg
        real::d1,d2
           if(ii>=ifps.and.ii<=ifpe.and.jj>=jfps.and.jj<=jfpe)then
               write(msg,'(a,2f14.8,a,2i6)')'grid node ',fxlong(ii,jj),fxlat(ii,jj),' index',ii,jj
               call message(msg)
           endif
       end subroutine display_clamp
    end subroutine fire_ignition_convert
     subroutine check_ignition_coordinate(x,x1,x2)
        real, intent(in)::x,x1,x2
        character(len=128)::msg
        if (.not.(x>x1 .and. x<x2))then
            write(msg,'(a,f14.8,a,2f14.8)')'ignition point coordinate',x,' must be inside the bounds',x1,x2
            call crash(msg)
        endif
     end subroutine check_ignition_coordinate
     subroutine clamp_to_grid(x,x1,x2,i1,i2,xout,iout)
        real, intent(in)::x,x1,x2
        integer, intent(in)::i1,i2
        real, intent(out)::xout
        integer, intent(out)::iout
        character(len=128)::msg
        integer:: i
        real::r,dx,xr
        dx=(x2-x1)/(i2-i1)
        r=i1+(x-x1)/dx
        iout=nint(r)
        xr=x1+(iout-i1)*dx
        if(fire_print_msg.ge.2)then
            write(msg,'(a,f14.8,a,f14.8,a,i6)')'coordinate ',x,' clamped to ',xr,' index',iout
            call message(msg)
        endif
        xout=xr
    end subroutine clamp_to_grid
     subroutine postprocess_lines(lines,value_name,config_flags)
        type(lines_type), intent(inout)::lines
        character(len=3), intent(in)::value_name
        TYPE (grid_config_rec_type) , INTENT(IN) :: config_flags
     integer::i,n
     real::value
     real::lat_ctr,lon_ctr
     character(len=128)msg,f2,f3
        n=lines%num_lines
        do i=1,n
            if(lines%line(i)%radius.gt.0.)lines%num_lines=i
            if(lines%line(i)%end_x.eq.0.)lines%line(i)%end_x=lines%line(i)%start_x
            if(lines%line(i)%end_y.eq.0.)lines%line(i)%end_y=lines%line(i)%start_y
            if(lines%line(i)%end_time.eq.0.)lines%line(i)%end_time=lines%line(i)%start_time
        enddo
    if(lines%longlat .eq. 0)then
       lines%unit_fxlong=1.
       lines%unit_fxlat=1.
    else
       lat_ctr=config_flags%cen_lat
       lon_ctr=config_flags%cen_lon
       lines%unit_fxlat=pi2/(360.*reradius)
       lines%unit_fxlong=cos(lat_ctr*pi2/360.)*lines%unit_fxlat
    endif
    if(fire_print_msg.ge.2)then
!$OMP CRITICAL(SFIRE_DRIVER_CRIT)
       if(lines%longlat .eq. 0)then
         write(msg,1)'start x','start y','end x','end y','start t','end t',value_name,'radius'
       else
         write(msg,1)'start lon','start lat','end lon','end lat','start time','end time',value_name,'radius'
       endif
1 format(4a10,4a9)
       call message(msg)
       do i=1,lines%num_lines
         select case (value_name)
              case ('ros')
                  value = lines%line(i)%ros
                  f2='(4f10.3,4f9.2)'
                  f3='(4f10.5,4f9.2)'
              case ('hfx')
                  value = lines%line(i)%hfx_value
                  f2='(4f10.3,2f9.2,e9.2,f9.2)'
                  f3='(4f10.5,2f9.2,e9.2,f9.2)'
              case default
                  call crash('postprocess_lines: bad value_name '//value_name)
         end select
         if(lines%longlat .eq. 0)then
           write(msg,f2)lines%line(i)%start_x, lines%line(i)%start_y, &
                      lines%line(i)%end_x, lines%line(i)%end_y, &
                      lines%line(i)%start_time, lines%line(i)%end_time, &
                      value, lines%line(i)%radius
         else
           write(msg,f3)lines%line(i)%start_x, lines%line(i)%start_y, &
                      lines%line(i)%end_x, lines%line(i)%end_y, &
                      lines%line(i)%start_time, lines%line(i)%end_time, &
                      value, lines%line(i)%radius
         endif
         call message(msg)
         if(lines%line(i)%start_time > lines%line(i)%end_time)then
              call crash('start time may not be after end time')
         endif
       enddo
!$OMP END CRITICAL(SFIRE_DRIVER_CRIT)
     endif
end subroutine postprocess_lines
subroutine fire_hfx_convert (config_flags,hfx)
    implicit none
    TYPE (grid_config_rec_type) , INTENT(IN) :: config_flags
    TYPE (lines_type), INTENT(OUT):: hfx
    integer::i
    logical:: real,ideal
    real::lat_ctr,lon_ctr
    character(len=128)msg
    hfx%num_lines=config_flags%fire_hfx_num_lines
    if(fire_max_lines.lt.hfx%num_lines)call crash('fire_max_lines too small')
    ideal=config_flags%fire_hfx_start_x1 .ne.0. .or. config_flags%fire_hfx_start_y1 .ne. 0.
    real=config_flags%fire_hfx_start_lon1 .ne. 0. .or. config_flags%fire_hfx_start_lat1 .ne. 0.
    if(ideal)call message('Using ideal heat flux line coordinates, m from the lower left domain corner')
    if(real)call message('Using real heat flux line coordinates, longitude and latitude')
    if(ideal.and.real)call crash('Only one of the ideal or real coordinates may be given')
    hfx%longlat=0
    if(ideal)then
       hfx%longlat=0
       hfx%line(1)%start_x=config_flags%fire_hfx_start_x1
       hfx%line(1)%start_y=config_flags%fire_hfx_start_y1
       hfx%line(1)%end_x=config_flags%fire_hfx_end_x1
       hfx%line(1)%end_y=config_flags%fire_hfx_end_y1
    endif
    if(real)then
       hfx%longlat=1
       hfx%line(1)%start_x=config_flags%fire_hfx_start_lon1
       hfx%line(1)%start_y=config_flags%fire_hfx_start_lat1
       hfx%line(1)%end_x=config_flags%fire_hfx_end_lon1
       hfx%line(1)%end_y=config_flags%fire_hfx_end_lat1
    endif
       hfx%line(1)%radius=config_flags%fire_hfx_radius1
       hfx%line(1)%start_time=config_flags%fire_hfx_start_time1
       hfx%line(1)%end_time=config_flags%fire_hfx_end_time1
       hfx%line(1)%trans_time=config_flags%fire_hfx_trans_time1
       hfx%line(1)%hfx_value=config_flags%fire_hfx_value1
       call postprocess_lines(hfx,'hfx',config_flags)
end subroutine fire_hfx_convert
subroutine set_flags(config_flags)
USE module_configure
implicit none
TYPE (grid_config_rec_type) , INTENT(IN) :: config_flags
fire_perimeter_time = config_flags%fire_perimeter_time
fire_tign_in_time = config_flags%fire_tign_in_time
fire_print_msg = config_flags%fire_print_msg
fire_print_file = config_flags%fire_print_file
fuel_left_method = config_flags%fire_fuel_left_method
fuel_left_irl = config_flags%fire_fuel_left_irl
fuel_left_jrl = config_flags%fire_fuel_left_jrl
fire_atm_feedback = config_flags%fire_atm_feedback
fire_hfx_given = config_flags%fire_hfx_given
fire_hfx_num_lines = config_flags%fire_hfx_num_lines
fire_hfx_latent_part = config_flags%fire_hfx_latent_part
fire_update_fuel_frac = config_flags%fire_update_fuel_frac
boundary_guard = config_flags%fire_boundary_guard
fire_back_weight = config_flags%fire_back_weight
fire_grows_only = config_flags%fire_grows_only
fire_upwinding = config_flags%fire_upwinding
fire_viscosity = config_flags%fire_viscosity
fire_lfn_ext_up = config_flags%fire_lfn_ext_up
fire_test_steps = config_flags%fire_test_steps
fire_advection = config_flags%fire_advection
fire_wind_log_interp = config_flags%fire_wind_log_interp
fire_use_windrf = config_flags%fire_use_windrf
fire_fmc_read = config_flags%fire_fmc_read
fire_ignition_clamp = config_flags%fire_ignition_clamp
kfmc_ndwi = config_flags%kfmc_ndwi
fndwi_from_ndwi = config_flags%fndwi_from_ndwi
fire_can_top_read = config_flags%fire_can_top_read
end subroutine set_flags
subroutine set_fp_from_grid(grid,fp)
    implicit none
    type(domain),intent(in)::grid
    type(fire_params),intent(out)::fp
    fp%vx => grid%uf
    fp%vy => grid%vf
    fp%zsf => grid%zsf
    fp%dzdxf => grid%dzdxf
    fp%dzdyf => grid%dzdyf
    fp%bbb => grid%bbb
    fp%phisc => grid%phisc
    fp%phiwc => grid%phiwc
    fp%r_0 => grid%r_0
    fp%fgip => grid%fgip
    fp%ischap => grid%ischap
    fp%fuel_time => grid%fuel_time
    fp%fmc_g => grid%fmc_g
    fp%nfuel_cat => grid%nfuel_cat
end subroutine set_fp_from_grid
subroutine print_id
character(len=128)::id,msg
id='commit 0da10de21de7940fc00171c12ca5984531b605a1 4 files changed, 8 insertions(+), 5 deletions(-)'
msg=id
call message(msg,level=1)
end subroutine print_id
end module module_fr_sfire_driver
