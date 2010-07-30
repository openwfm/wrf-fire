module module_model_test
use module_fr_sfire_util, only: message,crash
implicit none

contains 

subroutine sub_main

!*** purpose: standalone driver with compatible files to WRF-Fire

use module_fr_sfire_driver, only: sfire_driver_em
use module_domain, only: domain
use module_configure, only: grid_config_rec_type,read_namelist
use wrf_netcdf, only : grid_info, set_info_from_file, &
                       create_output_file,write_vars,output_vars, &
                       input_vars,read_vars
implicit none

!*** local

! arguments to SFIRE
type(domain)::grid          ! all: state+inputs+outputs, compatible with wrf
TYPE (grid_config_rec_type):: config_flags ! the namelist
integer::  &                ! atmosphere mesh dimensions, for compatibility
    ids,ide, kds,kde, jds,jde,& 
    ims,ime, kms,kme, jms,jme,&
    ips,ipe, kps,kpe, jps,jpe
integer::  &                ! fire mesh dimensions
    ifds,ifde,jfds,jfde, &  ! the physical domain
    ifps,ifpe,jfps,jfpe, &  ! patch - assigned to one process. Here the same as domain.
    ifms,ifme,jfms,jfme     ! memory allocated, needs a strip around the patch

! I/O interface
character(len=*),parameter::inputfile='fire_input.nc'
character(len=*),parameter::outputfile='fire_output.nc'
type(output_vars)::output                ! output arrays
type(input_vars)::input                  ! input arrays

! other derived types
type(grid_info)::info                    ! dimensions, grid controls

! scalars
integer:: nx,ny,nz,nfx,nfy,nfz,nsteps,istep
integer::nfhalo=3,nhalo=2
real:: dt, duration_s
logical::do_ouput

!*** executable

call read_namelist(config_flags)           ! read flags from namelist.input
call set_info_from_file(inputfile,info)     ! get dimensions

! set dimensions
nx=info%natmx
ny=info%natmy
nz=info%natmz
nfx=info%nfirex
nfy=info%nfirey

! set fire domain size
ifds=1
ifde=nfx
jfds=1
jfde=nfy
ifms=1-nfhalo
ifme=nfx+nfhalo
jfms=1-nfhalo
jfme=ny+nfhalo
ifps=1
ifpe=nfx
jfps=1
jfpe=nfy

! set atm domain size
ids=1
ide=nx
jds=1
jde=ny
kds=1
kde=ny
ims=1-nhalo
ime=nfx+nhalo
jms=1-nhalo
jme=ny+nhalo
kms=1
kme=nz
ips=1
ipe=nx
jps=1
jpe=ny
kps=1
kpe=nz

! coefficoent arrays to be passed to spread formulas, constant after initialized
! inputs
allocate(grid%uf(ifms:ifme,jfms:jfme))              ! fire winds
allocate(grid%vf(ifms:ifme,jfms:jfme))              ! fire winds
allocate(grid%zsf(ifms:ifme,jfms:jfme))             ! terrain height
allocate(grid%dzdxf(ifms:ifme,jfms:jfme))           ! terrain grad
allocate(grid%dzdyf(ifms:ifme,jfms:jfme))           ! terrain grad
allocate(grid%fxlong(ifms:ifme,jfms:jfme))          ! 
allocate(grid%fxlat(ifms:ifme,jfms:jfme))           !
allocate(grid%nfuel_cat(ifms:ifme,jfms:jfme))          ! 
! state
allocate(grid%bbb(ifms:ifme,jfms:jfme))             ! spread formula coeff
allocate(grid%betafl(ifms:ifme,jfms:jfme))          ! spread formula coeff
allocate(grid%phiwc(ifms:ifme,jfms:jfme))           ! spread formula coeff
allocate(grid%r_0(ifms:ifme,jfms:jfme))             ! spread formula coeff
allocate(grid%fgip(ifms:ifme,jfms:jfme))            ! spread formula coeff
allocate(grid%ischap(ifms:ifme,jfms:jfme))          ! spread formula coeff
allocate(grid%fuel_time(ifms:ifme,jfms:jfme))        ! 
allocate(grid%lfn(ifms:ifme,jfms:jfme)) 
allocate(grid%tign_g(ifms:ifme,jfms:jfme)) 
allocate(grid%fuel_frac(ifms:ifme,jfms:jfme)) 
allocate(grid%fire_area(ifms:ifme,jfms:jfme)) 
allocate(grid%lfn_out(ifms:ifme,jfms:jfme)) 
allocate(grid%ros(ifms:ifme,jfms:jfme)) 

! output
allocate(grid%fgrnhfx(ifms:ifme,jfms:jfme))          ! 
allocate(grid%fgrnqfx(ifms:ifme,jfms:jfme))          ! 
allocate(grid%fcanhfx(ifms:ifme,jfms:jfme))          ! 
allocate(grid%fcanqfx(ifms:ifme,jfms:jfme))          ! 

! atmosphere compatibility arrays
allocate(grid%grnhfx(ims:ime,jms:jme)) 
allocate(grid%grnqfx(ims:ime,jms:jme)) 
allocate(grid%u_2(ims:ime,jms:jme,kms:kme)) 
allocate(grid%v_2(ims:ime,jms:jme,kms:kme)) 

! read input file
input%nfuel_cat=>grid%nfuel_cat(ifps:ifpe,jfps:jfpe)
input%zsf      =>grid%zsf(ifps:ifpe,jfps:jfpe)
input%dzdxf    =>grid%dzdxf(ifps:ifpe,jfps:jfpe)
input%dzdyf    =>grid%dzdyf(ifps:ifpe,jfps:jfpe)
input%vf       =>grid%uf(ifps:ifpe,jfps:jfpe)
input%uf       =>grid%vf(ifps:ifpe,jfps:jfpe)
input%u        =>grid%u_2(ips:ipe,jps:jpe,kps:kpe)
input%v        =>grid%v_2(ips:ipe,jps:jpe,kps:kpe)

call read_vars(inputfile,info,input)

! set the scalars in grid type
grid%sr_x = info%sr_x
grid%sr_y = info%sr_y
grid%dt = info%dt
grid%dx = info%dx
grid%dy = info%dy

! time control
dt=info%dt
duration_s = config_flags%run_seconds           &
           + 60d0*(config_flags%run_minutes     &
           + 60d0*(config_flags%run_hours       &
           + 24d0*(config_flags%run_days)))       
nsteps = duration_s / dt  ! number of time steps

! divide up for shared memory parallel execution
call set_tiles(2,2,ips,ipe,jps,jpe,grid%num_tiles,grid%i_start,grid%i_end,grid%j_start,grid%j_end)

! start output file
call create_output_file(outputfile,info)

! initialize model
call sfire_driver_em ( grid , config_flags                          &
            ,1,3,0                                                 &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe )

do istep=1,nsteps
! run  model
   call sfire_driver_em ( grid , config_flags                          &
            ,1,3,0                                                 &
            ,ids,ide, kds,kde, jds,jde                              &
            ,ims,ime, kms,kme, jms,jme                              &
            ,ips,ipe, kps,kpe, jps,jpe                              &
            ,ifds,ifde, jfds,jfde                                   &
            ,ifms,ifme, jfms,jfme                                   &
            ,ifps,ifpe, jfps,jfpe )
    
    if(istep.le.10.or.mod(istep,10).eq.0)then
        output%lfn=>grid%lfn(ifps:ifpe,jfps:jfpe)
        output%tign=>grid%tign_g(ifps:ifpe,jfps:jfpe)
        output%grnhfx=>grid%grnhfx(ifps:ifpe,jfps:jfpe)
        call write_vars(outputfile,output,dt*istep)
    endif
enddo

end subroutine sub_main

!
!******************************
!

subroutine set_tiles(itiles,jtiles,ids,ide,jds,jde,num_tiles,i_start,i_end,j_start,j_end)
!*** set tiles for standalone/testing
implicit none
!*** arguments
integer,intent(in)::itiles,jtiles,ids,ide,jds,jde
integer,intent(out)::num_tiles
integer,intent(out),dimension(itiles*jtiles)::i_start,i_end,j_start,j_end
!*** local
integer::i,j,istep,jstep,ij
num_tiles=itiles*jtiles
istep=(ide-ids+itiles)/itiles
jstep=(jde-jds+jtiles)/jtiles
do i=1,itiles
    do j=1,jtiles
        ij=j+(i-1)*jtiles
        i_start(ij)=min(ide,ids+(i-1)*istep)
        i_end(ij)  =min(ide,ids+(i  )*istep-1)
        j_start(ij)=min(jde,jds+(j-1)*jstep)
        j_end(ij)  =min(jde,jds+(j  )*jstep-1)
    enddo
enddo
call check_tiles(ids,ide,jds,jde,num_tiles,i_start,i_end,j_start,j_end)
end subroutine set_tiles


subroutine check_tiles(ips,ipe,jps,jpe,num_tiles,i_start,i_end,j_start,j_end)
implicit none
!*** purpose: check if tiles fit
!*** arguments
integer,intent(in)::ips,ipe,jps,jpe,num_tiles
integer,intent(in),dimension(num_tiles)::i_start,i_end,j_start,j_end
!*** local
character(len=128)::msg
integer:: ij,ie
!*** executable
if(num_tiles.lt.1)call crash('check_tiles: need at least one tile')
ie=0
if (num_tiles.eq.1) then
    if(i_start(1).ne.ips.or.i_end(1).ne.ipe.or.j_start(1).ne.jps.or.j_end(1).ne.jpe)ie=1
else
    do ij=1,num_tiles
        if(i_start(ij).lt.ips.or.i_end(ij).gt.ipe &
        .or.j_start(ij).lt.jps.or.j_end(ij).gt.jpe)ie=ij
    enddo
endif
if(ie.ne.0)then        
    write(msg,*)'bad tile ',ie
    call message(msg)
    write(msg,*)'patch dimensions:',ips,ipe,jps,jpe
    call message(msg)
    do ij=1,num_tiles
        write(msg,*)'tile',ij,i_start(ij),i_end(ij),j_start(ij),j_end(ij)
        call message(msg)
    enddo
    call crash('bad tile bounds')
endif
end subroutine check_tiles

end module module_model_test

!
!******************************
!


program model_test_main
use module_model_test, only: sub_main
call  sub_main
end program model_test_main
