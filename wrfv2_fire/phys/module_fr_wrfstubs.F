module module_fr_wrfstubs
contains

subroutine crash(s)
use module_wrf_error
implicit none
character(len=*), intent(in)::s
character(len=128)msg
msg=s
call wrf_error_fatal(msg)
end subroutine crash

subroutine warning(s)
use module_wrf_error
implicit none
character(len=*), intent(in)::s
character(len=128)msg
msg=s
call wrf_debug(0,msg)
end subroutine warning


end module module_fr_wrfstubs
