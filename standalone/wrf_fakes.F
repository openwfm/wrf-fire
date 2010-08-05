! wrf_fakes.F
! things that would be normally done in WRF

module module_model_constants
! The model may not define any physical constants, all must be done here.
! copied what needed from WRFV3/share/module_model_constants.F
   REAL   , PARAMETER :: r_d          = 287.
   REAL   , PARAMETER :: cp           = 7.*r_d/2.   ! specific heat of the atmosphere
   REAL   , PARAMETER :: xlv          = 2.5E6       ! latent heat
   REAL   , PARAMETER ::  pi2=2.*3.1415926
   REAL   , PARAMETER :: reradius     = 1./6370.0e03
end module module_model_constants

!
!***
!

module module_wrf_error
implicit none
contains

! mock-up of various wrf utility functions

subroutine wrf_error_fatal(s)
!*** purpose: abort with a message
implicit none
character(len=*), intent(in)::s
call latch ! food for debugger so you can say "stop at latch" and not worry about module name
write(6,*)s
call abort()
end subroutine wrf_error_fatal

!
!***
!

subroutine wrf_debug(level,s)
!*** purpose: print a message

implicit none
character(len=*), intent(in)::s
integer, intent(in):: level
write(6,*)s
end subroutine wrf_debug

!
!***
!

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
1 write(6,'(a)')s(1:i)
end subroutine wrf_message

end module module_wrf_error

!
!***
!

! just for testing
subroutine latch
end

!
!*** various stubs, mostly doing nothing
!

module module_dm
use module_wrf_error
implicit none
contains
subroutine wrf_get_nproc (nprocs)
integer nprocs
nprocs=1
end subroutine wrf_get_nproc

!
!***
!

subroutine wrf_get_myproc( myproc )
integer myproc
myproc=1
end subroutine wrf_get_myproc

!
!***
!

subroutine wrf_dm_maxval_integer( val, idex, jdex )
integer::val
integer::idex,jdex
call wrf_error_fatal('wrf_dm_maxval_integer:not implemented')
end subroutine wrf_dm_maxval_integer

end module module_dm

!
!*** external stubs
!


SUBROUTINE wrf_dm_bcast_real( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   REAL  buf(*)
   RETURN
END SUBROUTINE wrf_dm_bcast_real

!
!***
!

LOGICAL FUNCTION wrf_dm_on_monitor()
  wrf_dm_on_monitor = .true.
END FUNCTION wrf_dm_on_monitor

!
!***
!

SUBROUTINE wrf_dm_bcast_integer( BUF, N1 )
   IMPLICIT NONE
   INTEGER n1
   INTEGER  buf(*)
   RETURN
END SUBROUTINE wrf_dm_bcast_integer
