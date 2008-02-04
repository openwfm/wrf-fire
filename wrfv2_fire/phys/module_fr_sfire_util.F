!
!*** Jan Mandel August-October 2007
!*** email: jmandel@ucar.edu or Jan.Mandel@gmail.com or Jan.Mandel@cudenver.edu
!
! This module contains general purpose utilities and WRF wrappers because I want the
! model to be able to run standalone. No physics here.
! Some are dependent on WRF indexing scheme. Some violate WRF conventions but these
! are not called from the WRF dependent code. Some are not called at all.
! 

module module_fr_sfire_util

contains

!
!****************
!
subroutine crash(s)
use module_wrf_error
implicit none
character(len=*), intent(in)::s
character(len=128)msg
msg='SFIRE:'//s
call wrf_error_fatal(msg)
end subroutine crash

!
!****************
!

subroutine message(s)
use module_wrf_error
implicit none
character(len=*), intent(in)::s
character(len=128)msg
msg='SFIRE:'//s
call wrf_message(msg)
end subroutine message

!
!****************
!

subroutine continue_at_boundary(ix,iy, & ! do x direction or y direction
    ims,ime,jms,jme, &                ! memory dims
    ids,ide,jds,jde, &                ! domain dims
    its,ite,jts,jte, &                ! tile dims
    lfn)                              ! array
implicit none
!*** description
! extend array by one beyond the domain by linear continuation
! if array is node based, call with ids jde ite jte larger by 1
!*** arguments
integer, intent(in)::ix,iy              ! not 0 = do x or y (1 or 2) direction
integer, intent(in)::ims,ime,jms,jme, &                ! memory dims
    ids,ide,jds,jde, &                ! domain dims
    its,ite,jts,jte                 ! tile dims
real,intent(inout),dimension(ims:ime,jms:jme)::lfn
!*** local
integer i,j
if(ix.ne.0)then
    if(its.eq.ids)then
        do j=jts,jte
            lfn(ids-1,j)=2*lfn(ids,j)-lfn(ids+1,j)
        enddo
    endif
    if(ite.eq.ide)then
        do j=jts,jte
            lfn(ide+1,j)=2*lfn(ide,j)-lfn(ide-1,j)
        enddo
    endif
endif
if(iy.ne.0)then
    if(jts.eq.jds)then
        do i=its,ite
            lfn(i,jds-1)=2*lfn(i,jds)-lfn(i,jds+1)
        enddo
    endif
    if(jte.eq.jde)then
        do i=its,ite
            lfn(i,jde+1)=2*lfn(i,jde)-lfn(i,jde-1)
        enddo
    endif
endif
end subroutine continue_at_boundary

!
!*****************************
!

subroutine check_mesh_2dim(ids,ide,jds,jde,ims,ime,jms,jme)
implicit none
integer, intent(in)::ids,ide,jds,jde,ims,ime,jms,jme
character(len=128)msg
if(ids<ims.or.ide>ime.or.jds<jms.or.jde>jme)then
    write(msg,*)'mesh dimensions:  ',ids,ide,jds,jde
    call message(msg)
    write(msg,*)'memory dimensions:',ims,ime,jms,jme
    call message(msg)
    call crash('check_mesh_2dim: memory dimensions too small')
endif
end subroutine check_mesh_2dim

!
!****************
!

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

!
!****************
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
        i_end(ij)  =min(ide,ids+(i  )*istep)
        j_start(ij)=min(jde,jds+(j-1)*jstep)
        j_end(ij)  =min(jde,jds+(j  )*jstep)
    enddo
enddo
call check_tiles(ids,ide,jds,jde,num_tiles,i_start,i_end,j_start,j_end)
end subroutine set_tiles

!
!****************
!

pure integer function enode(ite,ipe)
    implicit none
    integer, intent(in)::ite,ipe
! purpose: find the upper bound for tile loops for nodal array
! Nodal arrays are by 1 larger than cell arrays, so the upper bound for tile
! loops has to be increased by one if the tile is at the upper end of the domain.
! The upper bound cannot be increased by 1 always because then two tile loops would try
! to write into the same memory location.   
    if(ite.ne.ipe)then ! most frequent, first
        enode=ite
    else
        enode=ite+1
    endif
end function enode

!
!****************
!

pure integer function snode(its,ips)
    implicit none
    integer, intent(in)::its,ips
! purpose: similar to enode, but for the start
    if(its.ne.ips)then ! most frequent, first
        snode=its
    else
        snode=its-1
    endif
end function snode

!
!****************
!

subroutine check_mesh_3dim(ids,ide,kds,kde,jds,jde,ims,ime,kms,kme,jms,jme)
integer, intent(in)::ids,ide,jds,jde,ims,ime,jms,jme,kds,kde,kms,kme
if(ids<ims.or.ide>ime.or.jds<jms.or.jde>jme.or.kds<kms.or.kde>kme) then
    call crash('memory dimensions too small')
endif
end subroutine check_mesh_3dim

!
!****************
!

subroutine sum_2d_cells(       &
       ims2,ime2,jms2,jme2,    &
       its2,ite2,jts2,jte2,    &
       v2,                     &  ! input       
       ims1,ime1,jms1,jme1,    &
       its1,ite1,jts1,jte1,    &
       v1)                        ! output
implicit none

!*** purpose
! sum cell values in mesh2 to cell values of coarser mesh1

!*** arguments
! the dimensions are in cells, not nodes!

integer, intent(in)::its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)

!*** local
integer:: i1,i2,j1,j2,ir,jr,isz1,isz2,jsz1,jsz2,ioff,joff
real t
character(len=128)msg

!*** executable

!check mesh dimensions and domain dimensions
call check_mesh_2dim(its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1)
call check_mesh_2dim(its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2)

! compute mesh sizes
isz1 = ite1-its1+1
jsz1 = jte1-jts1+1
isz2 = ite2-its2+1
jsz2 = jte2-jts2+1

! check mesh sizes
if(isz1.le.0.or.jsz1.le.0.or.isz2.le.0.or.jsz2.le.0)then
    call message('all mesh sizes must be positive')
    goto 9
endif
if(mod(isz2,isz1).ne.0.or.mod(jsz2,jsz1).ne.0)then
    call message('input mesh size must be multiple of output mesh size')
    goto 9
endif

! compute mesh ratios
ir=isz2/isz1
jr=jsz2/jsz1

! v1 = v1 + sum(v2)
do j1=jts1,jte1
    do i1=its1,ite1
       t=0
       do joff=0,jr-1
           do ioff=0,ir-1
               i2=ioff+its2+ir*(i1-its1)
               j2=joff+jts2+jr*(j1-jts1)
               t=t+v2(i2,j2)
           enddo
       enddo
       v1(i1,j1)=t
    enddo
enddo

return
9 continue
write(msg,91)its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
call message(msg)
write(msg,91)its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
call message(msg)
write(msg,92)'input  mesh size:',isz2,jsz2
call message(msg)
91 format('dimensions: ',8i8)
write(msg,92)'output mesh size:',isz1,jsz1
call message(msg)
92 format(a,2i8)
call crash('module_fr_spread_util:sum_mesh_cells: bad mesh sizes')
end subroutine sum_2d_cells

!
!****************
!

subroutine interpolate_2d_nodes2nodes3(  &
    ids2,ide2,kds2,kde2,jds2,jde2, & ! dimensions coarse grid
    ims2,ime2,kms2,kme2,jms2,jme2, & ! array coarse grid
    ids1,ide1,jds1,jde1,kds1,kde1, & ! dimensions fine grid
    ims1,ime1,jms1,jme1,kms1,kme1, & ! array coarse grid
    ir,jr,               & ! refinement ratio
    ip2,jp2,ip1,jp1,     & ! (ip2,jp2) on grid 2 lines up with (ip1,jp1) on grid 1 
    v2, &                  ! in coarse grid  
    v1  )                  ! out fine grid
implicit none

!*** purpose
! interpolate nodal values in mesh2 to nodal values in mesh1
! input mesh 2 is coarse output mesh 1 is fine
! interpolation runs over mesh2 region ids2:ide2,jds2:jde2 
! only the part of mesh 1 region ids1:ide1,jds1:jde1 is modified

!*** arguments

integer, intent(in)::ids1,ide1,jds1,jde1,kds1,kde1,ims1,ime1,jms1,jme1,kms1,kme1
integer, intent(in)::ids2,ide2,kds2,kde2,jds2,jde2,ims2,ime2,kms2,kme2,jms2,jme2
integer, intent(in)::ir,jr,ip2,jp2,ip1,jp1 
real, intent(out)::v1(ims1:ime1,jms1:jme1,kms1:kme1)
real, intent(in)::v2(ims2:ime2,kms2:kme2,jms2:jme2)

!*** local
integer:: i1,i2,j1,j2,is,ie,js,je,io,jo,k1,k2
real:: tx,ty,rx,ry

!*** executable

!check mesh dimensions and domain dimensions
call check_mesh_2dim(ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1)
call check_mesh_2dim(ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2)

! compute mesh ratios
rx=ir
ry=jr

do k1=kds1,kds2
k2=(k1-kds1)+kds2
do j2=jds2,jde2-1
    jo=jp1+jr*(j2-jp2)
    js=max(jds1,jo)
    je=min(jde1,jo+jr)
    do i2=ids2,ide2-1
        io=ip1+ir*(i2-ip2)
        is=max(ids1,io)
        ie=min(ide1,io+ir)
        do j1=js,je
            ty = (j1-jo)/ry
            do i1=is,ie
                tx = (i1-io)/rx
                v1(i1,j1,k1)=                     &
                      (1-tx)*(1-ty)*v2(i2,k2,j2)  &
                 +    (1-tx)*ty  *v2(i2,k2,j2+1)  &
                 +      tx*(1-ty)*v2(i2+1,k2,j2)  &
                 +        tx*ty  *v2(i2+1,k2,j2+1)  
                !pr`int *,'coarse ',i2,j2,' fine ',i1,j1, ' offset ',io,jo,' weights ',tx,ty, &
                ! 'in ',v2(i2,j2),v2(i2,j2+1),v2(i2+1,j2),v2(i2+1,j2+1),' out ',v1(i1,j1)
           enddo
       enddo
    enddo
enddo
enddo

end subroutine interpolate_2d_nodes2nodes3


! module_fr_sfire_util%%interpolate_2d_nodes2nodes
subroutine interpolate_2d_nodes2nodes(  &
    ims2,ime2,jms2,jme2, & ! array coarse grid
    its2,ite2,jts2,jte2, & ! dimensions coarse grid
    ims1,ime1,jms1,jme1, & ! array coarse grid
    its1,ite1,jts1,jte1, & ! dimensions fine grid
    ir,jr,               & ! refinement ratio
    ip2,jp2,ip1,jp1,     & ! (ip2,jp2) on grid 2 lines up with (ip1,jp1) on grid 1 
    v2, &                  ! in coarse grid  
    v1  )                  ! out fine grid
implicit none

!*** purpose
! interpolate nodal values in mesh2 to nodal values in mesh1
! input mesh 2 is coarse output mesh 1 is fine
! interpolation runs over mesh2 region its2:ite2,jts2:jte2 
! only the part of mesh 1 region its1:ite1,jts1:jte1 is modified

!*** arguments

integer, intent(in)::its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1
integer, intent(in)::its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2
integer, intent(in)::ir,jr,ip2,jp2,ip1,jp1 
real, intent(out)::v1(ims1:ime1,jms1:jme1)
real, intent(in)::v2(ims2:ime2,jms2:jme2)

!*** local
integer:: i1,i2,j1,j2,is,ie,js,je,io,jo
real:: tx,ty,rx,ry

!*** executable

!check mesh dimensions and domain dimensions
call check_mesh_2dim(its1,ite1,jts1,jte1,ims1,ime1,jms1,jme1)
call check_mesh_2dim(its2,ite2,jts2,jte2,ims2,ime2,jms2,jme2)

! compute mesh ratios
rx=ir
ry=jr

do j2=jts2,jte2-1
    jo=jp1+jr*(j2-jp2)
    js=max(jts1,jo)
    je=min(jte1,jo+jr)
    do i2=its2,ite2-1
        io=ip1+ir*(i2-ip2)
        is=max(its1,io)
        ie=min(ite1,io+ir)
        do j1=js,je
            ty = (j1-jo)/ry
            do i1=is,ie
                tx = (i1-io)/rx
                !print *,'coarse ',i2,j2,'to',i2+1,j2+1,' fine ',is,js,' to ',ie,je
                v1(i1,j1)=                     &
                      (1-tx)*(1-ty)*v2(i2,j2)  &
                 +    (1-tx)*ty  *v2(i2,j2+1)  &
                 +      tx*(1-ty)*v2(i2+1,j2)  &
                 +        tx*ty  *v2(i2+1,j2+1)  
                !print *,'coarse ',i2,j2,' fine ',i1,j1, ' offset ',io,jo,' weights ',tx,ty, &
                ! 'in ',v2(i2,j2),v2(i2,j2+1),v2(i2+1,j2),v2(i2+1,j2+1),' out ',v1(i1,j1)
           enddo
       enddo
    enddo
enddo

end subroutine interpolate_2d_nodes2nodes

!
!****************
!

subroutine interpolate_2d_cells2cells(              &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  ! in  
      ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1  )  ! out
implicit none

!*** purpose
! interpolate nodal values in mesh2 to nodal values in mesh1
! input mesh 2 is coarse output mesh 1 is fine

!*** arguments

integer, intent(in)::ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)

! Example with mesh ratio=4. | = cell boundary,  x = cell center
!
!  mesh2   |-------x-------|-------x-------|
!  mesh1   |-x-|-x-|-x-|-x-|-x-|-x-|-x-|-x-| 
!

!*** local
integer:: ir,jr,isz1,isz2,jsz1,jsz2,ip,jp,ih,jh
character(len=128)msg

!*** executable

!check mesh dimensions and domain dimensions
call check_mesh_2dim(ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1)
call check_mesh_2dim(ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2)

! compute mesh sizes
isz1 = ide1-ids1+1
jsz1 = jde1-jds1+1
isz2 = ide2-ids2+1
jsz2 = jde2-jds2+1

! check mesh sizes
if(isz1.le.0.or.jsz1.le.0.or.isz2.le.0.or.jsz2.le.0)goto 9
if(mod(isz1,isz2).ne.0.or.mod(jsz1,jsz2).ne.0)goto 9

! compute mesh ratios
ir=isz1/isz2
jr=jsz1/jsz2
!
!  mesh2   |-------x-------|-------x-------|
!  mesh1   |-x-|-x-|-x-|-x-|-x-|-x-|-x-|-x-| 

!  mesh2   |-----x-----|-----x-----|  rx=3
!  mesh1   |-x-|-x-|-x-|-x-|-x-|-x-| 
!  i2            1   1   1   2
!  i1        1   2   3   4   5
!  ioff          0   1   2   0
!  tx            0  1/3 2/3

!  mesh2   |---x---|---x---| rx=2
!  mesh1   |-x-|-x-|-x-|-x-| 
!  i2            1   1   2  
!  i1            2   3   4
!  ioff          0   1   2   
!  tx           1/4 3/4


! offset of the last node in the 1st half of the cell
ih=ir/2
jh=jr/2
! 0 if coarse cell center coincides with fine, 1 if not
ip=mod(ir+1,2)
jp=mod(jr+1,2)

call interpolate_2d_w(ip,jp,ih,jh,ir,jr,              &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  ! in  
      ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1  )  ! out

return

9 continue
write(msg,91)ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
call message(msg)
write(msg,91)ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
call message(msg)
write(msg,92)'input  mesh size:',isz2,jsz2
call message(msg)
91 format('dimensions: ',8i8)
write(msg,92)'output mesh size:',isz1,jsz1
call message(msg)
92 format(a,2i8)
call crash("module_fr_sfire_util:interpolate_2dmesh_cells: bad mesh sizes")
end subroutine interpolate_2d_cells2cells

!
!****************
!

subroutine interpolate_2d_cells2nodes(              &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  ! in  
      ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1  )  ! out
implicit none

!*** purpose
! interpolate nodal values in mesh2 to nodal values in mesh1
! input mesh 2 is coarse output mesh 1 is fine

!*** arguments

integer, intent(in)::ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)

! Example with mesh ratio=4. | = cell boundary,  x = cell center
!
!  mesh2   |-------x-------|-------x-------|
!  mesh1   x-|-x-|-x-|-x-|-x-|-x-|-x-|-x-|-x 
!

!*** local
integer:: ir,jr,isz1,isz2,jsz1,jsz2,ip,jp,ih,jh
character(len=128)msg

!*** executable

!check mesh dimensions and domain dimensions
call check_mesh_2dim(ids1,ide1+1,jds1,jde1+1,ims1,ime1,jms1,jme1)
call check_mesh_2dim(ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2)

! compute mesh sizes
isz1 = ide1-ids1+1
jsz1 = jde1-jds1+1
isz2 = ide2-ids2+1
jsz2 = jde2-jds2+1

! check mesh sizes
if(isz1.le.0.or.jsz1.le.0.or.isz2.le.0.or.jsz2.le.0)goto 9
if(mod(isz1,isz2).ne.0.or.mod(jsz1,jsz2).ne.0)goto 9

! compute mesh ratios
ir=isz1/isz2
jr=jsz1/jsz2
!
!  mesh2   |-------x-------|-------x-------|
!  mesh1   x-|-x-|-x-|-x-|-x-|-x-|-x-|-x-|-x 

!  mesh2   |-----x-----|-----x-----|  rx=3
!  mesh1   x-|-x-|-x-|-x-|-x-|-x-|-x 

!  mesh2   |---x---|---x---| rx=2
!  mesh1   x-|-x-|-x-|-x-|-x 

! offset of the last node in the 1st half of the cell
ih=(ir+1)/2
jh=(jr+1)/2
! 0 if coarse cell center coincides with fine, 1 if not
ip=mod(ir,2)
jp=mod(jr,2)


call interpolate_2d_w(ip,jp,ih,jh,ir,jr,              &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  ! in  
      ids1,ide1+1,jds1,jde1+1,ims1,ime1,jms1,jme1,v1  )  ! out


return
9 continue
write(msg,91)ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
call message(msg)
write(msg,91)ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
call message(msg)
write(msg,92)'input  mesh size:',isz2,jsz2
call message(msg)
91 format('dimensions: ',8i8)
write(msg,92)'output mesh size:',isz1,jsz1
call message(msg)
92 format(a,2i8)
call crash("module_fr_sfire_util:interpolate_2d_cells2nodes: bad mesh sizes")
end subroutine interpolate_2d_cells2nodes
!
!****************
!

subroutine interpolate_2d_w(ip,jp,ih,jh,ir,jr,             &
      ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2,v2, &  ! in  
      ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1,v1  )  ! out
implicit none
!*** EXCEPTION: THIS SUBROUTINE IS NEITHER CELL NOR NODE BASED.

integer, intent(in)::ip,jp,ih,jh,ir,jr
integer, intent(in)::ids1,ide1,jds1,jde1,ims1,ime1,jms1,jme1
real, intent(out)::v1(ims1:ime1,jms1:jme1)
integer, intent(in)::ids2,ide2,jds2,jde2,ims2,ime2,jms2,jme2
real, intent(in)::v2(ims2:ime2,jms2:jme2)

real:: tx,ty,rx,ry,half,xoff,yoff
integer:: i1,i2,j1,j2,ioff,joff
parameter(half=0.5)

rx=ir
ry=jr

xoff = ip*half
yoff = jp*half

! the inside, ids1+ih:ide1-ih,jds1+jh:jde1-jh 
do j2=jds2,jde2-1     ! interpolate from nodes j2 and j2+1
    do i2=ids2,ide2-1
        do ioff=0,ir-ip
            do joff=0,jr-jp
                ! compute fine mesh index
                i1=ioff+(ih+ids1)+ir*(i2-ids2)
                j1=joff+(jh+jds1)+jr*(j2-jds2)
                ! weights
                tx = (ioff+xoff)/rx
                ty = (joff+yoff)/ry
                ! interpolation
                v1(i1,j1)=                     &
                      (1-tx)*(1-ty)*v2(i2,j2)  &
                 +    (1-tx)*ty  *v2(i2,j2+1)  &
                 +      tx*(1-ty)*v2(i2+1,j2)  &
                 +        tx*ty  *v2(i2+1,j2+1)  
                !write(*,'(3(a,2i5),a,2f7.4)')'coarse ',i2,j2,' fine ',i1,j1, &
                ! ' offset ',ioff,joff,' weights ',tx,ty
                !write(*,'(a,4f7.4,a,f7.4)')'in ',v2(i2,j2),v2(i2,j2+1),v2(i2+1,j2), &
                !  v2(i2+1,j2+1),' out ',v1(i1,j1)
           enddo
       enddo
    enddo
enddo

! extend to the boundary strips from the nearest known
do ioff=0,ih-1  ! top and bottom strips
    do j2=jds2,jde2-1
        do joff=0,jr-jp
           j1=joff+(jh+jds1)+jr*(j2-jds2)
           ! weights
           ty = (joff+yoff)/ry
           ! interpolation
           v1(ids1+ioff,j1)=(1-ty)*v2(ids2,j2)+ty*v2(ids2,j2+1)
           v1(ide1-ioff,j1)=(1-ty)*v2(ide2,j2)+ty*v2(ide2,j2+1)
       enddo
    enddo
enddo
do joff=0,jh-1  ! left and right strips
    do i2=ids2,ide2-1
        do ioff=0,ir-ip
           i1=ioff+(ih+ids1)+ir*(i2-ids2)
           ! weights
           tx = (ioff+xoff)/rx
           ! interpolation
           v1(i1,jds1+joff)=(1-tx)*v2(i2,jds2)+tx*v2(i2+1,jds2)
           v1(i1,jde1-joff)=(1-tx)*v2(i2,jde2)+tx*v2(i2+1,jde2)
       enddo
    enddo
enddo
! extend to the 4 corner squares from the nearest known
do ioff=0,ih-1  
    do joff=0,jh-1
        v1(ids1+ioff,jds1+joff)=v2(ids2,jds2)
        v1(ide1-ioff,jds1+joff)=v2(ide2,jds2)
        v1(ids1+ioff,jde1-joff)=v2(ids2,jde2)
        v1(ide1-ioff,jde1-joff)=v2(ide2,jde2)
    enddo
enddo         
end subroutine interpolate_2d_w  

!
!****************
!
                
real function interp(ids,ide,jds,jde,ims,ime,jms,jme,x,y,v)
implicit none
!*** purpose
! general interpolation in a rectangular

!*** arguments

integer, intent(in)::ids,ide,jds,jde,ims,ime,jms,jme
real, intent(in)::x,y,v(ims:ime,jms:jme)
! the mesh is cell based so the used dimension of v is ids:ide+1,jds:jde+1

!*** calls
intrinsic floor,min,max

!*** local
integer i,j
real tx,ty

! executable

! indices of the lower left corner of the cell in the mesh that contains (x,y)
i = floor(x)
i=max(min(i,ide),ids)
j = floor(y)
j=max(min(j,jde),jds)

! the leftover
tx = x - real(i)
ty = y - real(j)

! interpolate the values
interp = &
                    (1-tx)*(1-ty)*v(i,j)    &
                 +    tx*(1-ty)  *v(i+1,j)  &
                 +    (1-tx)*ty  *v(i,j+1)  &
                 +        tx*ty  *v(i+1,j+1)  

!print *,'x,y=',x,y,'i1,i2=',i1,j1,'tx,ty=',tx,ty,' interp=',interp
end function interp

subroutine meshdiffc_2d(ids, ide, jds,jde , &    ! mesh area used (in cells, end +1)
                   ims1,ime1,jms1,jme1, &       ! memory dimensiuons 
                   dx,dy,               &       ! mesh spacing
                   lfn,                 &       ! input
                   diffCx,diffCy) ! output
implicit none

!*** purpose
! central differences on a 2d mesh

!*** arguments

integer, intent(in)::ids,ide,jds,jde,ims1,ime1,jms1,jme1
real, intent(in):: dx,dy
real, intent(in), dimension(ims1:ime1,jms1:jme1):: lfn
real, intent(out), dimension(ims1:ime1,jms1:jme1):: diffCx,diffCy

!*** local
integer:: i,j
real, dimension(ims1:ime1,jms1:jme1):: diffLx,diffRx,diffLy,diffRy

! get one-sided differences; dumb but had that already...
call meshdiff_2d(ids, ide, jds,jde , &    ! mesh area used (in cells, end +1)
                   ims1,ime1,jms1,jme1, &       ! dimensions of lfn 
                   dx,dy,               &       ! mesh spacing
                   lfn,                 &       ! input
                   diffLx,diffRx,diffLy,diffRy) ! output

! make into central
do j=jds,jde+1
    do i=ids,ide+1
        diffCx(i,j)=0.5*(diffLx(i,j) + diffRx(i,j))
        diffCy(i,j)=0.5*(diffLy(i,j) + diffRy(i,j))
    enddo
enddo
end subroutine meshdiffc_2d 

subroutine meshdiff_2d(ids, ide, jds,jde , &    ! mesh area used (in cells, end +1)
                   ims1,ime1,jms1,jme1, &       ! dimensions of lfn 
                   dx,dy,               &       ! mesh spacing
                   lfn,                 &       ! input
                   diffLx,diffRx,diffLy,diffRy) ! output
implicit none

!*** purpose
! one-sided differences on a 2d mesh

!*** arguments

integer, intent(in)::ids,ide,jds,jde,ims1,ime1,jms1,jme1
real, intent(in):: dx,dy
real, intent(in), dimension(ims1:ime1,jms1:jme1):: lfn
real, intent(out), dimension(ims1:ime1,jms1:jme1):: diffLx,diffRx,diffLy,diffRy

!*** local
integer:: i,j
real:: tmpx,tmpy

!*** executable

    call check_mesh_2dim(ids,ide+1,jds,jde+1,ims1,ime1,jms1,jme1)
  
    ! the bulk of the work
    do j=jds,jde
        do i=ids,ide
            tmpx = (lfn(i+1,j)-lfn(i,j))/dx
            diffLx(i+1,j) = tmpx
            diffRx(i,j)   = tmpx
            tmpy = (lfn(i,j+1)-lfn(i,j))/dy
            diffLy(i,j+1) = tmpy
            diffRy(i,j)   = tmpy
        enddo
        ! missing values - put there the other one
        diffLx(ids,j)  = diffLx(ids+1,j)
        diffRx(ide+1,j)= diffRx(ide,j)
    enddo
    ! cleanup
    ! j=jde+1 from above loop
    do i=ids,ide
        tmpx = (lfn(i+1,j)-lfn(i,j))/dx
        diffLx(i+1,j) = tmpx
        diffRx(i,j)   = tmpx
    enddo
    ! i=ide+1 from above loop
    do j=jds,jde
        tmpy = (lfn(i,j+1)-lfn(i,j))/dy
        diffLy(i,j+1) = tmpy
        diffRy(i,j)   = tmpy
    enddo
    ! missing values - put there the other one
    ! j=jde+1 from above loop, j=jds:jde done before in main bulk loop
    diffLx(ids,j)   = diffLx(ids+1,j)
    diffRx(ide+1,j) = diffRx(ide,j)
    do i=ids,ide+1
        diffLy(i,jds)   = diffLy(i,jds+1)
        diffRy(i,jde+1) = diffRy(i,jde)
    enddo    

end subroutine meshdiff_2d




real pure function sum_2darray( its,ite,jts,jte,               &
                                ims,ime,jms,jme,               &
                                a)
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
real, intent(in)::a(ims:ime,jms:jme)
!*** local
integer:: i,j
real:: t
t=0.
do j=jts,jte
    do i=its,ite
        t=t+a(i,j)
    enddo
enddo
sum_2darray = t
end function sum_2darray

real pure function max_2darray( its,ite,jts,jte,               &
                                ims,ime,jms,jme,               &
                                a)
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
real, intent(in)::a(ims:ime,jms:jme)
!*** local
integer:: i,j
real:: t
t=0.
do j=jts,jte
    do i=its,ite
        t=max(t,a(i,j))
    enddo
enddo
max_2darray = t
end function max_2darray

subroutine print_2d_stats_vec(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         ax,ay,name)
implicit none
integer, intent(in)::ips,ipe,jps,jpe,ims,ime,jms,jme
real, intent(in), dimension(ims:ime,jms:jme)::ax,ay
character(len=*),intent(in)::name
integer:: i,j
real:: t 
real:: avg_a,max_a,min_a 
character(len=25)id
id=name
call print_2d_stats(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         ax,id//'/x ')
call print_2d_stats(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         ay,id//'/y ')
avg_a=0
max_a=-huge(max_a)
min_a= huge(min_a)
do j=jps,jpe
    do i=ips,ipe
        t=sqrt(ax(i,j)**2+ay(i,j)**2)
        max_a=max(max_a,t)
        min_a=min(min_a,t)
        avg_a=avg_a+t
    enddo
enddo
avg_a = avg_a/((ipe-ips+1)*(jpe-jps+1))
call print_stat_line(id//'/sz',min_a,max_a,avg_a)
end subroutine print_2d_stats_vec


subroutine print_stat_line(name,min_a,max_a,avg_a)
!*** encapsulate line with statistics
implicit none
!*** arguments
character(len=*),intent(in)::name
real,intent(in)::min_a,max_a,avg_a
!*** local
character(len=128)msg
character(len=28)id
id=name
write(msg,'(a,3(a,g10.3))')id,':min ',min_a,' max ',max_a,' avg ',avg_a
call message(msg)
if(.not.avg_a.eq.avg_a)call crash('NaN detected')
end subroutine print_stat_line


subroutine print_3d_stats(ips,ipe,kps,kpe,jps,jpe, &
                         ims,ime,kms,kme,jms,jme, &
                         a,name)
implicit none
integer, intent(in)::ips,ipe,jps,jpe,ims,ime,jms,jme,kms,kme,kps,kpe
real, intent(in)::a(ims:ime,kms:kme,jms:jme)
character(len=*),intent(in)::name
integer:: i,j,k
real:: avg_a,max_a,min_a 
avg_a=0
max_a=-huge(max_a)
min_a= huge(min_a)
do j=jps,jpe
  do k=kps,kpe
    do i=ips,ipe
        max_a=max(max_a,a(i,k,j))
        min_a=min(min_a,a(i,k,j))
        avg_a=avg_a+a(i,k,j)
    enddo
  enddo
enddo
avg_a = avg_a/((ipe-ips+1)*(jpe-jps+1)*(kpe-kps+1))
call print_stat_line(name,min_a,max_a,avg_a)
end subroutine print_3d_stats

subroutine print_2d_stats(ips,ipe,jps,jpe, &
                         ims,ime,jms,jme, &
                         a,name)
implicit none
integer, intent(in)::ips,ipe,jps,jpe,ims,ime,jms,jme
real, intent(in)::a(ims:ime,jms:jme)
character(len=*),intent(in)::name
call print_3d_stats(ips,ipe,1,1,jps,jpe, &
                         ims,ime,1,1,jms,jme, &
                         a,name)
end subroutine print_2d_stats

real pure function avg_2darray( its,ite,jts,jte,               &
                                ims,ime,jms,jme,               &
                                a)
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
real, intent(in)::a(ims:ime,jms:jme)
!*** local
!*** executable
avg_2darray = sum_2darray( its,ite,jts,jte,               &
                           ims,ime,jms,jme,               &
                           a)/((ite-its+1)*(jte-jts+1))
end function avg_2darray

real pure function avg_2darray_vec( its,ite,jts,jte,           &
                                ims,ime,jms,jme,               &
                                ax,ay)
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme
real, intent(in), dimension(ims:ime,jms:jme):: ax,ay
!*** local
integer:: i,j
real:: t
t=0.
do j=jts,jte
    do i=its,ite
        t=t+sqrt(ax(i,j)**2+ay(i,j)**2)
    enddo
enddo
t = t/((ite-its+1)*(jte-jts+1))
avg_2darray_vec = t
end function avg_2darray_vec


subroutine write_array_m(its,ite,jts,jte,           &
                         ims,ime,jms,jme,               &
                         a,name,id)
! debug
!*** arguments
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme,id
real, intent(in), dimension(ims:ime,jms:jme):: a
character(len=*),intent(in)::name
!****
call write_array_m3(its,ite,1,1,jts,jte,           &
                         ims,ime,1,1,jms,jme,               &
                         a,name,id)
end subroutine write_array_m


subroutine write_array_m3(its,ite,kts,kte,jts,jte,           &
                         ims,ime,kms,kme,jms,jme,               &
                         a,name,id)
! debug
!*** arguments
integer, intent(in)::its,ite,jts,jte,ims,ime,jms,jme,kts,kte,kms,kme,id
real, intent(in), dimension(ims:ime,kms:kme,jms:jme):: a
character(len=*),intent(in)::name
!****
integer i,j,k,iu,ilen
logical op
character(len=128)::fname,msg
!****
call check_mesh_2dim(its,ite,jts,jte,ims,ime,jms,jme)
do iu=6,99
    inquire(unit=iu,opened=op)
    if(.not.op)goto 17
enddo
call crash('write_array_m: cannot find available fortran unit')
17 write(fname,3)name,'_',id,'.txt'
do ilen=len(fname),2,-1
    if(fname(ilen:ilen).ne.' ')goto 19
enddo
19 continue
open(iu,file=fname(1:ilen))
write(iu,1)real(ite-its+1)
write(iu,1)real(jte-jts+1)
write(iu,1)real(kte-kts+1)
write(iu,1)(((a(i,k,j),i=its,ite),j=jts,jte),k=kts,kte)
close(iu)
write(msg,2)name,'(',its,':',ite,',',jts,':',jte,',', &
kts,':',kte,') -> ',fname(1:ilen) 
call message(msg)
1 format(e20.12)
2 format(2a,3(i5,a,i5,a),2a)
3 format(a,a,i5.5,a)
end subroutine write_array_m3


end module module_fr_sfire_util
