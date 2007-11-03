module module_wrf_error
contains

subroutine wrf_error_fatal(s)
character(len=*), intent(in)::s
call latch
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

subroutine latch
end

