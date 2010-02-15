! -*- f90 -*-
subroutine get_projection_coords(idomain,ix,iy,lon,lat)
use gridinfo_module
use llxy_module
implicit none

integer,intent(in)::idomain
real,intent(in)::ix,iy
real,intent(out)::lon,lat


call get_grid_params()
call compute_nest_locations()
call select_domain(idomain)
call xytoll(ix,iy,lon,lat,0)
end subroutine get_projection_coords

subroutine parallel_abort()
implicit none
print*,'problem in libgeogrid.a'
end subroutine parallel_abort
