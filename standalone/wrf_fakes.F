module module_model_constants
! copied what needed from share/module_model_constants.F
   REAL    , PARAMETER :: r_d          = 287.
   REAL    , PARAMETER :: cp           = 7.*r_d/2.   ! specific heat of the atmosphere
   REAL    , PARAMETER :: xlv          = 2.5E6       ! latent heat
end module module_model_constants

module module_wrf_error
contains

subroutine wrf_error_fatal(s)
character(len=*), intent(in)::s
call latch ! food for debugger so you can say "stop at latch"
print *,s
call abort()
end subroutine wrf_error_fatal

subroutine wrf_debug(level,s)
character(len=*), intent(in)::s
integer, intent(in):: level
print *,s
end subroutine wrf_debug

subroutine wrf_message(s)
character(len=*), intent(in)::s
integer i
do i=len(s),2,-1
    select case(s(i:i))
    case(' ')
    case default
        goto 1
    end select
enddo
1 write(*,'(a)')s(1:i)
end subroutine wrf_message

end module module_wrf_error

module module_dm
contains
subroutine wrf_get_nproc (nprocs)
integer nprocs
nprocs=1
end subroutine wrf_get_nproc

subroutine wrf_get_myproc( myproc )
integer myproc
myproc=1
end subroutine wrf_get_myproc

end module module_dm

! just for testing
subroutine latch
end

! external stubs, from module_dm 
SUBROUTINE wrf_dm_bcast_real( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   REAL  buf(*)
   RETURN
END SUBROUTINE wrf_dm_bcast_real

LOGICAL FUNCTION wrf_dm_on_monitor()
  wrf_dm_on_monitor = .true.
END FUNCTION wrf_dm_on_monitor

SUBROUTINE wrf_dm_bcast_integer( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   INTEGER  buf(*)
   RETURN
END SUBROUTINE wrf_dm_bcast_integer
