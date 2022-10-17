real function f(x) result(y)
  implicit none
  real, intent(in) :: x
  y = 2*exp(x)-2*x**2-4
end function f

subroutine secant(xo, x1, eps, x2, u)
  implicit none
  real, intent(in) :: eps
  real, intent(inout) :: xo
  real, external :: f
  real :: x1, x2, dif
  integer :: icount = 1, u
  if (abs(f(x1)-f(xo))<tiny(xo)) then
    print *, 'Error, failure algorithm'
    x2 = 0
  else
    x2 = x1 - f(x1)*(x1-xo)/(f(x1)-f(xo))
    dif = abs(x2-x1)
    do while ( dif > eps )
      write (u,'(i3, f9.5)') icount, x1
      icount = icount + 1
      if (abs(f(x1)-f(xo))<tiny(xo)) exit
      x2 = x1 - f(x1)*(x1-xo)/(f(x1)-f(xo))
      dif = abs(x2-x1)
      xo = x1
      x1 = x2
    end do
  end if
end subroutine secant

subroutine bisection(a,b,c, eps,u, ios)
  implicit none
  real :: a, b, c, f, alk, dif, eps
  integer :: icount
  integer :: u, ios
  c = (a+b)/2
  icount = 1
  dif = b - c
  do while (abs(dif) > eps)
    alk = sign(1.0,f(b)) * sign(1.0,f(c))
    !print *, equation(0.0)
    if (ios == 0) then
      write(u,'(i3, f8.5)') icount, c
    else
      print *, 'File error'
    end if
    if (alk <= 0) then
      a=c
    else
      b=c
    end if
    c = (a+b)/2
    dif = b - c
    !print *, dif
    icount = icount + 1
  end do
  !print *, 'bisection took ', icount, ' iterations.'
end subroutine bisection


program last
  implicit none
  real :: xo, eps, x1, x2
  real :: xo_b, x1_b, x2_b
  character(len=20) :: file_name='secant.csv'
  integer :: u=10 , ios
  write(*,*) 'Input the xo value in'
  read *, xo
  write(*,*) 'Input the x1 value'
  read *, x1
  write(*,*) 'Input the precision'
  read *, eps
  xo_b = xo
  x1_b = x1
  open(unit=u, iostat=ios, file=file_name, status='replace', action='write')
  write(u,*) 'Secant Method'
  call secant(xo, x1, eps, x2, u)
  write(u,*) 'Bisection Method'
  call bisection(xo_b, x1_b, x2_b, eps,u, ios)
  !write (*,*) x2
  !The fastest method is secant method
end program last
