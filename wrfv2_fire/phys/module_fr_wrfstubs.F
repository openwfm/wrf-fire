module module_fr_wrfstubs
contains

subroutine crash(s)
use module_wrf_error
implicit none
character(len=*), intent(in)::s
character(len=128)msg
msg='FR:'//s
call wrf_error_fatal(msg)
end subroutine crash

subroutine message(s)
use module_wrf_error
implicit none
character(len=*), intent(in)::s
character(len=128)msg
msg='FR:'//s
call wrf_message(msg)
end subroutine message


end module module_fr_wrfstubs
