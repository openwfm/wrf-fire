module module_wrf_error
contains

subroutine wrf_error_fatal(s)
character(len=*), intent(in)::s
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
print *,s
end subroutine wrf_message

end module module_wrf_error

