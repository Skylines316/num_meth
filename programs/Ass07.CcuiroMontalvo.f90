real function f(x) result(y)
  implicit none
  real, intent(in) :: x
  y = 2*exp(x)-2*x**3-3
end function f

real function Df(x) result (y)
  implicit none
  real, intent(in) :: x
  y = 2*exp(x)-6*x**2
end function Df

subroutine raphson(xo, eps, x1)
  implicit none
  real, intent(in) :: eps
  real, intent(inout) :: xo
  real, external :: f, Df
  real :: x1, dif
  integer :: icount = 1
  character(len=20) :: file_name='newton_raphson.csv'
  integer :: u=10, ios
  open(unit=u, iostat=ios, file=file_name, status='replace', action='write')
  !print *, tiny(xo), Df(xo)
  if (abs(Df(xo))<tiny(xo)) then
    print *, 'Error, failure algorithm'
    x1 = 0
  else
    x1 = xo-f(xo)/Df(xo)
    dif = abs(x1-xo)
    do while ( dif > eps )
      write (u,'(i3, f9.5)') icount, xo
      icount = icount + 1
      x1 = xo-f(xo)/Df(xo)
      dif = abs(x1-xo)
      xo = x1
    end do
  end if
  close(u)
end subroutine raphson

program newton
  implicit none
  real :: xo, eps, x1
  write(*,*) 'Estimate the value of xo, has to be negative'
  read *,xo !the initial value has to be less than 0 because there are positive roots
  write(*,*) 'Input value of the precision'
  read *, eps
  call raphson(xo, eps, x1)
  write (*,*) x1
  !if I want all the roots I should change my xo and put a value near to every root
end program newton
