MODULE assig_3

CONTAINS

REAL(8) FUNCTION f(x,t) RESULT(res)
  implicit none
  real(8), intent(in) :: x
  real(8), intent(in) :: t
  res = x
END FUNCTION f

subroutine euler(t, y, yo, n, h)
  implicit none
  real(8) :: h
  integer, intent(in) :: n
  integer :: i
  real(8), dimension(n) :: y , t
  real(8) :: yo
  !array (to,to+h,to+2h,...)
  !array (x(to)=xo, x(t1=to+h)=xo+h*f(to)=x1, x(t2=to+2h)=x1+h*f(t1), ...)
  y(1) = yo
  do i=2,n
    y(i) = y(i-1) + h*f(y(i-1),t(i))
    !print *, a
  end do
end subroutine euler

subroutine midpoint(t, y, yo, n, h)
  implicit none
  real(8) :: h
  integer, intent(in) :: n
  integer :: i
  real(8), dimension(n) :: y , t
  real(8) :: yo
  !array (to,to+h,to+2h,...)
  !array (x(to)=xo, x(t1=to+h)=xo+h*f(to)=x1, x(t2=to+2h)=x1+h*f(t1), ...)
  y(1) = yo
  do i=2,n
    y(i) = y(i-1) + h*f(y(i-1)+h/2*f(y(i-1),t(i)),t(i)+h/2)
    !print *, a
  end do
end subroutine midpoint

END MODULE assig_3


PROGRAM assignment_int_3
  use assig_3
  implicit none
  real(8) :: h, yo, to
  real(8), dimension(:), allocatable :: t,y,te,ye
  integer :: i , n
  h = 0.05
  n = 100
  to = 0
  allocate(t(n), y(n), te(n), ye(n))
  do i = 1,n
    t(i) = to + h*(i-1)
  end do
  yo = 1
  call euler(t, y, yo, n, h)
  te = t
  ye = y
  call midpoint(t, y, yo, n, h)
  open (unit=10, file='data.dat', status='replace')
  do i = 1,n
  write (10, '(f8.3,f8.3,f8.3,f8.3)') te(i), ye(i), t(i), y(i)
  end do
  close(10)
END PROGRAM assignment_int_3
