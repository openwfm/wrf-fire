module m
real, pointer:: p(:)
contains
subroutine y(n)
implicit none
integer n,i
do i=1,n
p(i)=i*i
enddo
end subroutine y
end module m

subroutine a(n)
use m
implicit none
integer n,i
real, target:: p_t(n)
! at this point p_t has memory but no values set yet
! g95 complains that p_t used but not set 
p => p_t
! set p inside
call y(n)
print *,(p(i),i=1,n)
end subroutine a

program x
use m
implicit none
call a(3)
!call y(3)
end program x
