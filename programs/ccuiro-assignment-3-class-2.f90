MODULE assig_3

CONTAINS

REAL(8) FUNCTION f(a, b, x, t, y) RESULT(res)
  implicit none
  real(8), intent(in) :: x, y
  real(8), intent(in) :: t
  real(8) :: a, b
  res = a*x-b*x*y
END FUNCTION f

REAL(8) FUNCTION g(d, ga, y, t, x) RESULT(res)
  implicit none
  real(8), intent(in) :: x, y
  real(8), intent(in) :: t
  real(8) :: d, ga
  res = d*x*y-ga*y
END FUNCTION g

subroutine midpoint(t, x, xo, y, yo, n, h)
  implicit none
  real(8) :: h
  integer, intent(in) :: n
  integer :: i
  real(8), dimension(n) :: x,y , t
  real(8) :: yo, xo
  real(8) :: a, b, d, ga
  !array (to,to+h,to+2h,...)
  !array (x(to)=xo, x(t1=to+h)=xo+h*f(to)=x1, x(t2=to+2h)=x1+h*f(t1), ...)
  write (*,*) "Input alpha, beta, delta, gamma"
  read *, a
  read *, b
  read *, d
  read *, ga
  y(1) = yo
  x(1) = xo
  do i=2,n
    x(i) = x(i-1) + h*f(&
                        a, b, &
                        x(i-1)+h/2*f(a, b, x(i-1), t(i), y(i-1)), &
                        t(i)+h/2, &
                        y(i-1)+h/2*g(d, ga, y(i-1), t(i), x(i-1))&
                        )
    y(i) = y(i-1) + h*g(&
                        d, ga, &
                        y(i-1)+h/2*g(d, ga, y(i-1), t(i), x(i-1)), &
                        t(i)+h/2, &
                        x(i-1)+h/2*f(a, b, x(i-1), t(i), y(i-1)) &
                        )
    !print *, a
  end do
end subroutine midpoint

END MODULE assig_3


PROGRAM assignment_int_3
  use assig_3
  implicit none
  real(8) :: h, yo, to, xo
  real(8), dimension(:), allocatable :: t,y,x
  integer :: i , n
  h = 0.1
  n = 100
  to = 0
  allocate(t(n), y(n), x(n))
  do i = 1,n
    t(i) = to + h*(i-1)
  end do
  xo = 1.0
  yo = 0.6
  call midpoint(t, x, xo, y, yo, n, h)
  open (unit=10, file='lotka.dat', status='replace')
  do i = 1,n
  write (10, '(f8.3,f8.3,f8.3,f8.3)') t(i), x(i), y(i)
  end do
  close(10)
END PROGRAM assignment_int_3
